
---
title: "Ring0的“消息”——IRP"
date: 2014-06-01T09:13:30+08:00
draft: true
categories: ["Windows安全"]
tags: ["Windows安全"]
topics: ["Windows安全"]
---

IRP由IO管理器创建用来代表一个IO操作，它在ring0的地位类似**消息**于win32应用程序的意义，是信息交互的单位。IRP是学习Windows驱动开发的基础，这里介绍一下IRP的结构、请求方式以及IRP在分层驱动中的传递。

## IRP请求包结构

<!--more-->

wdm.h中定义了IRP结构：


```

typedef struct DECLSPEC_ALIGN(MEMORY_ALLOCATION_ALIGNMENT) _IRP {
    CSHORT Type;
    USHORT Size;

    //
    // Define the common fields used to control the IRP.
    //

    //
    // Define a pointer to the Memory Descriptor List (MDL) for this I/O
    // request.  This field is only used if the I/O is "direct I/O".
    //

    PMDL MdlAddress;

    //
    // Flags word - used to remember various flags.
    //

    ULONG Flags;

    //
    // The following union is used for one of three purposes:
    //
    //    1. This IRP is an associated IRP.  The field is a pointer to a master
    //       IRP.
    //
    //    2. This is the master IRP.  The field is the count of the number of
    //       IRPs which must complete (associated IRPs) before the master can
    //       complete.
    //
    //    3. This operation is being buffered and the field is the address of
    //       the system space buffer.
    //

    union {
        struct _IRP *MasterIrp;
        __volatile LONG IrpCount;
        PVOID SystemBuffer;
    } AssociatedIrp;

    //
    // Thread list entry - allows queueing the IRP to the thread pending I/O
    // request packet list.
    //

    LIST_ENTRY ThreadListEntry;

    //
    // I/O status - final status of operation.
    //

    IO_STATUS_BLOCK IoStatus;

    //
    // Requestor mode - mode of the original requestor of this operation.
    //

    KPROCESSOR_MODE RequestorMode;

    //
    // Pending returned - TRUE if pending was initially returned as the
    // status for this packet.
    //

    BOOLEAN PendingReturned;

    //
    // Stack state information.
    //

    CHAR StackCount;
    CHAR CurrentLocation;

    //
    // Cancel - packet has been canceled.
    //

    BOOLEAN Cancel;

    //
    // Cancel Irql - Irql at which the cancel spinlock was acquired.
    //

    KIRQL CancelIrql;

    //
    // ApcEnvironment - Used to save the APC environment at the time that the
    // packet was initialized.
    //

    CCHAR ApcEnvironment;

    //
    // Allocation control flags.
    //

    UCHAR AllocationFlags;

    //
    // User parameters.
    //

    PIO_STATUS_BLOCK UserIosb;
    PKEVENT UserEvent;
    union {
        struct {
            union {
                PIO_APC_ROUTINE UserApcRoutine;
                PVOID IssuingProcess;
            };
            PVOID UserApcContext;
        } AsynchronousParameters;
        LARGE_INTEGER AllocationSize;
    } Overlay;

    //
    // CancelRoutine - Used to contain the address of a cancel routine supplied
    // by a device driver when the IRP is in a cancelable state.
    //

    __volatile PDRIVER_CANCEL CancelRoutine;

    //
    // Note that the UserBuffer parameter is outside of the stack so that I/O
    // completion can copy data back into the user's address space without
    // having to know exactly which service was being invoked.  The length
    // of the copy is stored in the second half of the I/O status block. If
    // the UserBuffer field is NULL, then no copy is performed.
    //

    PVOID UserBuffer;

    //
    // Kernel structures
    //
    // The following section contains kernel structures which the IRP needs
    // in order to place various work information in kernel controller system
    // queues.  Because the size and alignment cannot be controlled, they are
    // placed here at the end so they just hang off and do not affect the
    // alignment of other fields in the IRP.
    //

    union {

        struct {

            union {

                //
                // DeviceQueueEntry - The device queue entry field is used to
                // queue the IRP to the device driver device queue.
                //

                KDEVICE_QUEUE_ENTRY DeviceQueueEntry;

                struct {

                    //
                    // The following are available to the driver to use in
                    // whatever manner is desired, while the driver owns the
                    // packet.
                    //

                    PVOID DriverContext[4];

                } ;

            } ;

            //
            // Thread - pointer to caller's Thread Control Block.
            //

            PETHREAD Thread;

            //
            // Auxiliary buffer - pointer to any auxiliary buffer that is
            // required to pass information to a driver that is not contained
            // in a normal buffer.
            //

            PCHAR AuxiliaryBuffer;

            //
            // The following unnamed structure must be exactly identical
            // to the unnamed structure used in the minipacket header used
            // for completion queue entries.
            //

            struct {

                //
                // List entry - used to queue the packet to completion queue, among
                // others.
                //

                LIST_ENTRY ListEntry;

                union {

                    //
                    // Current stack location - contains a pointer to the current
                    // IO_STACK_LOCATION structure in the IRP stack.  This field
                    // should never be directly accessed by drivers.  They should
                    // use the standard functions.
                    //

                    struct _IO_STACK_LOCATION *CurrentStackLocation;

                    //
                    // Minipacket type.
                    //

                    ULONG PacketType;
                };
            };

            //
            // Original file object - pointer to the original file object
            // that was used to open the file.  This field is owned by the
            // I/O system and should not be used by any other drivers.
            //

            PFILE_OBJECT OriginalFileObject;

        } Overlay;

        //
        // APC - This APC control block is used for the special kernel APC as
        // well as for the caller's APC, if one was specified in the original
        // argument list.  If so, then the APC is reused for the normal APC for
        // whatever mode the caller was in and the "special" routine that is
        // invoked before the APC gets control simply deallocates the IRP.
        //

        KAPC Apc;

        //
        // CompletionKey - This is the key that is used to distinguish
        // individual I/O operations initiated on a single file handle.
        //

        PVOID CompletionKey;

    } Tail;

} IRP;

typedef IRP *PIRP;

```


实际上IRP结构体仅仅是一个请求的头，也叫IRP头。IRP在设备栈传递，在IRP后面还有IRP的数据部分：IO_STACK_LOCATION，每一层都对应一个IO_STACK_LOCATION。


```

typedef struct _IO_STACK_LOCATION {
    UCHAR MajorFunction;
    UCHAR MinorFunction;
    UCHAR Flags;
    UCHAR Control;

    //
    // The following user parameters are based on the service that is being
    // invoked.  Drivers and file systems can determine which set to use based
    // on the above major and minor function codes.
    //

    union {

    //
    // 每一种请求都能在这里找到对应的参数
    //
    } Parameters;

    //
    // Save a pointer to this device driver's device object for this request
    // so it can be passed to the completion routine if needed.
    //

    PDEVICE_OBJECT DeviceObject;

    //
    // The following location contains a pointer to the file object for this
    // request.
    //

    PFILE_OBJECT FileObject;

    //
    // The following routine is invoked depending on the flags in the above
    // flags field.
    //

    PIO_COMPLETION_ROUTINE CompletionRoutine;

    //
    // The following is used to store the address of the context parameter
    // that should be passed to the CompletionRoutine.
    //

    PVOID Context;

} IO_STACK_LOCATION, *PIO_STACK_LOCATION;

```


每个IRP请求包都包括一个IRP头以及一个由很多IO_STACK_LOCATION组成的数组或叫栈，IRP头的CurrentLocation指明当前实在数组的哪一层。

IO管理器将请求封装成IRP包（IRP+[IO_STACK_LOCATION]），在设备栈传递，交给设备栈的每一个设备的驱动程序来处理。

## 三种请求方式

不同的设备接收不同类型的IO请求，有三种请求方式：Buffered、Direct(In/Out)、其他。在DEVICE_OBJECT的Flags标志中设置对应的为：DO_BUFFERED_IO、DO_DIRECT_IO或0。因为操作方式是在IRP头中设置的，所以创建设备、绑定设备形成设备栈时要注意Flags标志的一致。为了方便讨论，这里假设定义了一个PIRP变量irp和PIO_STACK_LOCATION变量irpsp。

Buffered方式相对是最安全IO请求方式，同时也是比较慢的一种。应用层的数据传入时，IO管理器将其复制到irp-> SystemBuffer。IO管理器负责创建SystemBuffer，将其作为ring3和ring0的中介。

Direct方式相比Buffered更加快速一些，这时IO管理器并不会创建一个缓冲区来做中介，而是锁定用户缓冲区的内存页，并创建一个内存描述符MDL irp-> MdlAddress来表示。这时可以用MmGetSystemAddressForMdlSafe获取MDL描述的内存。

其他方式直接使用irp-> UserBuffer，直接将用户模式地址传进来。由于进程切换等问题，这种方式并不安全，也不建议使用。

在这三种方法中，读写长度都可以从irpsp-> Parameters.Read.Length和irp-> Parameters.Write.Length获取，读或写的数据长度，要通过填写irp-> IoStatus.Information来通知应用层。

### IOCTL的情况

使用DeviceIoControl与设备交互时，需要指定操作模式:METHOD_BUFFERED\METHOD_IN_DIRECT\METHOD_OUT_DIRECT\METHOD_NETHER。使用CTL_CODE( DeviceType, Function, Method, Access)设置IO控制码。

应用层使用DeviceIoControl与驱动通信，会发送IRP_MJ_DEVICE_CONTROL类型的IRP，这时的处理方式略有不同。

缓冲模式IOCTL同样是使用irp-> SystemBuffer做输入输出缓冲区；直接内存模式IOCTL的输入缓冲区是irp-> SystemBuffer，输出缓冲区用MDL描述；METHOD_NETHIER输入缓冲区是irpsp-> Parameters.DeviceIoControl.Type3InputBuffer，输出缓冲区是irp-> UserBuffer。

缓冲区大小都可以在irp-> Parameters.DeviceIoControl中得到。

## IRP在驱动间传递

IRP相当于消息，所以需要在驱动间传递。IoCallDriver向其他设备发送IRP。这里的传递是单向的，一旦传出去，这个IRP就跟当前驱动没关系了，也就不能在操作它了。

当前设备接收到IRP时，使用IoGetCurrentIrpStackLocation获取当前层的IO_STACK_LOCATION结构，可以进行一些适当的操作。完成操作之后有三种选择：直接完成这个请求，即不再向下传递；直接传到下一层，由下一层处理；传到下一层，设置一个完成函数期望再次处理IRP。前面提到一旦传出去就不能再操作这个IRP了，所以完成函数是唯一重新获取IRP控制权的途径。

要完成这个IRP，可以使用IoCompleteRequest完成请求，不再向下传递。

IoSkipCurrentIrpLocation将CurrentStack加1，从而弥补了IoCallDriver时的减1，相当于下一层依然使用这些参数，之后返回IoCallDriver的结果，这一层不再关心下面是怎么处理这个IRP的。

要想重新获得IRP控制，需要调用IoCopyCurrentIrpStackLocationToNext复制当前IO_STACK_LOCATION栈到下一层然后再调用IoSetCompetionRoutine设置完成函数，当下一层处理完IRP时完成函数被调用，当前驱动重新获得IRP控制。

设置完成函数时，需要自己设置Peding位，调用IoMarkIrpPending设置当前IRP为未完成。如果完成函数返回STATUS_SUCCESS或者STATUS_CONTINUE_COMPLETION，则继续调用上一层设置的完成函数，继续**回卷**；如果返回STATUS_MORE_PROCESSING_REQUIRED，则暂停回卷，IRP重新被当前驱动控制，可以选择向下传递或者完成请求。

有一段很经典的代码可以表达这一精妙的操作：


```

{
	Kevent event;
	KeInitializeEvent(&event, NotificatinoEvent, FALSE);
	IoCopyCurrentIrpStackLocationToNext(Irp);
	IoSetCompletionRoutine(
		Irp,
		IrpComplete,
		&event,
		TRUE,
		TRUE,
		TRUE
	);
	status = IoCallDriver(DeviceObject, Irp);
	if(status == STATUS_PENDING)
	{
　　	status = KeWaitForSingleObject(
		&event,
		Executive,
                KernelMode,
                FALSE,
                NULL);
	}
	/*
	Do something.
	*/
}

NTSTATUS IrpComplete(
    IN PDEVICE_OBJECT DeviceObject,
    IN PIRP Irp,
    IN PVOID Context
)
{
    if(Irp-> PendingReturned)
    {
        KeSetEvent( (PKEVENT)Context, IO_NO_INCREMENT, FALSE);
    }
    return STATUS_MORE_PROCESSING_REQUIRED;
}

```


## IRP的其他方面

IRP在windows内核中有很重要的地位，关于它的问题也有很多，同步、异步等，需要在实践中慢慢琢磨。
