---
title: "RocketMQ原理分享"
date: 2018-08-11T09:13:30+08:00
draft: true
categories: ["后端架构"]
tags: ["后端架构", "源码分析"]
topics: ["后端架构", "源码分析"]
---

之前在组内分享过一次RocketMQ，虽然写了PPT，还是口述+黑板画图，现场分享效果更佳。RocketMQ架构清晰，只有NameServer、Broker、Producer和Consumer四个组件，通过共享commitlog，即使数十万topic，依然能保持高性能；是通过工程巧妙解决业务难题的典范。RocketMQ真正做到了“简单”，例如重复消费这种功能，完全抛给消费侧；通过构建逻辑队列，每个消息都会指定唯一队列，解决消息顺序的问题。

下面介绍整体架构和四个组件，然后分析一条消息从诞生到被消费的全过程，从中了解RocketMQ设计的精妙。

<!--more-->

### 整体架构

![pic1](/images/rocketmq1.png "")

NameServer在内存中保存broker路由信息，broker定期向其发送心跳包，保证连接；NameServer存储的路由包括：topic及其队列(包括集群信息)、brokername和机器ip（master/slave）、cluster name和broker name信息、broker探活信息等。

broker存储数据，包括消息、consumer和订阅关系、消费偏移、延迟队列消费进度、topic信息（namesrv保存的是topic的集群信息，具体数据都在broker）。broker和namesrv之间有长连接。

producer和consumer从namesrv获取broker地址并向broker发送请求。

### Broker

每台机器在配置文件中指定相同的broker_name，名字相同的组成master+slave结构的broker。主要功能如下：

* 消息持久化 —— 后面着重介绍
* 保存下面这些重要信息，slave会定时请求master同步：
    * topic：读写queue的数量和权限等（每个topic都由多个queue组成，每个queue有queueid）
    * consumer group：包括组内消费者、订阅关系、消费进度、消费方式（是否广播模式、pull或push）、消费失败重试次数、broker切换等
    * producer group：缓存消费组对应的生产者信息

![pic2](/images/rocketmq2.png "")

### NameServer

NameServer作为名字中心，管理broker路由信息、topic（与其对应queue）集群信息，producer和consumer通过它获取broker地址。

* 无状态，只在内存中保存数据；且节点之间没有相互通信；重启之后，只需等待broker重新注册即可
* broker与所有nameserver保持长连接，定期更新自身信息
* 对broker有存活检测机制，摘除不活跃的broker地址

```
public class RouteInfoManager {
    private static final InternalLogger log = InternalLoggerFactory.getLogger(LoggerName.NAMESRV_LOGGER_NAME);
    private final static long BROKER_CHANNEL_EXPIRED_TIME = 1000 * 60 * 2;
    private final ReadWriteLock lock = new ReentrantReadWriteLock();
    private final HashMap<String/* topic */, List<QueueData>> topicQueueTable;
    private final HashMap<String/* brokerName */, BrokerData> brokerAddrTable;
    private final HashMap<String/* clusterName */, Set<String/* brokerName */>> clusterAddrTable;
    private final HashMap<String/* brokerAddr */, BrokerLiveInfo> brokerLiveTable;
    private final HashMap<String/* brokerAddr */, List<String>/* Filter Server */> filterServerTable;
    ...
}
```

### Producer

Producer启动时需要指定nameserver地址，并与其中一个server建立长连接。定期从nameserver获取topic对应的broker路由信息，与所有master节点建立长连接。

```
public void updateTopicRouteInfoFromNameServer() {
    Set<String> topicList = new HashSet<String>();

    ...

    // Producer
    {
        Iterator<Entry<String, MQProducerInner>> it = this.producerTable.entrySet().iterator();
        while (it.hasNext()) {
            Entry<String, MQProducerInner> entry = it.next();
            MQProducerInner impl = entry.getValue();
            if (impl != null) {
                Set<String> lst = impl.getPublishTopicList();
                topicList.addAll(lst);
            }
        }
    }

    for (String topic : topicList) {
        this.updateTopicRouteInfoFromNameServer(topic);
    }
}
```

Producer生产消息需指定topic+queueid，queueid由producer发送时指定，兼具负载均衡和顺序发送。

```
public static TopicPublishInfo topicRouteData2TopicPublishInfo(final String topic, final TopicRouteData route) {
    TopicPublishInfo info = new TopicPublishInfo();
    info.setTopicRouteData(route);
    if (route.getOrderTopicConf() != null && route.getOrderTopicConf().length() > 0) {
        String[] brokers = route.getOrderTopicConf().split(";");
        for (String broker : brokers) {
            String[] item = broker.split(":");
            int nums = Integer.parseInt(item[1]);
            for (int i = 0; i < nums; i++) {
                MessageQueue mq = new MessageQueue(topic, item[0], i);
                info.getMessageQueueList().add(mq);
            }
        }

        info.setOrderTopic(true);
    } else {
        ...
    }

    return info;
}
```

### Consumer

Consumer连接Broker和NameServer与Producer相同，也在`updateTopicRouteInfoFromNameServer`。获取消息有两种模式：pull和push，不过最终都是类似pull的方式，拉取消息。

Consumer从broker获取Consumer Group内所有消费者数量，以及topic的MessageQueueList，在本地做负载均衡；消费进度Offset会保存在本地作为参考，定期与broker同步；消费失败或其它异常，broker会把消息放到重试队列或者死信队列。

### 消息流

消息从“生”到“死”的整个过程如下，为保证完整，topic创建等内容也有所涉及。

#### topic创建

包括topic创建、消费组定义以及订阅关系产生。

管理工具发起创建topic命令，指定cluster和queue数量、topic对应queue数量；然后请求NameServer获取broker地址；通过UPDATE_AND_CREATE_TOPIC请求，broker更新config并向NameServer注册，后者保存topic信息。

多主架构中，每个master都有独立的topic队列，共同组成全量数据。

```
fetchMasterAddrByClusterName -> createTopic -> registerBroker
```

#### 消息发送

生产者调用getRouteInfoByTopic(GET_ROUTEINTO_BY_TOPIC请求)从NameServer获取topic路由信息TopicRouteData;然后调用topicRouteData2TopicPublishInfo方法得到包含MessageQueue list的TopicPublishInfo。

```
public class TopicRouteData extends RemotingSerializable {
    private String orderTopicConf;
    private List<QueueData> queueDatas;
    private List<BrokerData> brokerDatas;
    private HashMap<String/* brokerAddr */, List<String>/* Filter Server */> filterServerTable;
    ...
}
```

```
public class QueueData implements Comparable<QueueData> {
    private String brokerName;
    private int readQueueNums;
    private int writeQueueNums;
    private int perm;
    private int topicSynFlag;
    ...
}
```

MessageQueue与master*queue number对应，selectOneMessageQueue通过轮询方式选择要把消息发往哪个broker的哪个queue。

#### 消息存储

每台master有一个commitlog，所有topic共享，收到消息后写入，并定期刷盘（多种刷盘策略）；后台线程定期对每个topic生成consume squeue，存储commitlog偏移，供消费者消费（取出offset，然后读commitlog）；利用定长的内存映射文件（1G），写入速度非常快，读操作也只是读内存。

上层所谓offset偏移，一般指queue队列偏移，而非commitlog文件偏移。

```
public class DefaultMessageStore implements MessageStore {
    private static final InternalLogger log = InternalLoggerFactory.getLogger(LoggerName.STORE_LOGGER_NAME);
    private final MessageStoreConfig messageStoreConfig;
    // CommitLog
    private final CommitLog commitLog;
    private final ConcurrentMap<String/* topic */, ConcurrentMap<Integer/* queueId */, ConsumeQueue>> consumeQueueTable;
    private final FlushConsumeQueueService flushConsumeQueueService;
    private final CleanCommitLogService cleanCommitLogService;
    private final CleanConsumeQueueService cleanConsumeQueueService;
    private final IndexService indexService;
    private final AllocateMappedFileService allocateMappedFileService;
    private final ReputMessageService reputMessageService;
    private final HAService haService;
    private final ScheduleMessageService scheduleMessageService;
    private final StoreStatsService storeStatsService;
    private final TransientStorePool transientStorePool;
    ...
}
```

ReputMessageService的doReput方法定期把commitlog写入，实际执行写入的是CommitLogDispatcherBuildConsumeQueue类。

commitlog文件结构如下，多种刷盘策略：同步刷盘或异步刷盘，同时CommitRealTimeService定期写入fileChannel（并不是立即刷盘，会有自己的缓存）。

![pic3](/images/rocketmq3.png "")

Checkpoint文件记录commitlog、consume queue刷盘时间，用于恢复数据。

#### 高可用

Rocketmq高可用通过master broker和slave broker同步数据（topic配置、消费进度、订阅关系）实现，slave从nameserver获取master地址，定期请求master获取config和消息（两者同步频率不同）；master根据同步策略向slave同步发送消息或者定期更新消息内容。

#### 消息消费

消费者通过rebalance模块实现负载均衡，获取属于自己的consume queue，并创建对应的数据请求：从nameserver获取broker地址，然后请求数据，将数据放入ProcessQueue作为缓存。

ConsumeMessageService服务将缓存的数据发送给用户，并更新offset；消息拉取、负载均衡、消息消费三个步骤，都是通过一个PullRequest队列异步完成的。新的消费者或broker加入时，会通知所有消费者，重新执行rebalance操作。

![pic4](/images/rocketmq4.png "")

### RocketMQ其它特色

* 事务消息：通过 prepare消息——执行事务——commit消息三个阶段完成，以写入consume queue为准
* 延时消费：线程定时扫描不同粒度的延时队列，写入consume queue
* 全局commitlog：读写分离，topic数量不受限制
* 广播消费：消费组管理offset，没有消息重试
* 顺序消费：利用tag字段，producer写入同一个queue，consumer用锁保证消费
