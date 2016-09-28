+++
date = "2016-09-23T02:46:25+08:00"
title = "nginx_source"

+++

ngx_basic:
1. ngx_palloc.c ngx_palloc.h
2. ngx_array.c ngx_array.h
3. ngx_queue.c ngx_queue.h
4. ngx_list.c ngx_list.h
5. ngx_string.c ngx_string.h
6. ngx_rbtree.c ngx_rbtree.h
7. ngx_radix_tree.c ngx_radix_tree.h
8. ngx_hash.c ngx_hash.h

ngx_system_about:
1. ngx_errno.c ngx_errno.h
2. ngx_file.h ngx_file.c
3. ngx_open_file_cache.c ngx_open_file_cache.h
4. ngx_buf.c ngx_buf.h
5. ngx_log.c ngx_log.h
6. ngx_sys_log.c ngx_sys_log.h
7. ngx_times.c ngx_times.h
8. ngx_spinlock.c
9. ngx_shmtx.c ngx_shmtx.h
10. ngx_slab.c ngx_slab.h
11. ngx_rwlock.c ngx_rwlock.h
12. ngx_thread_pool.c ngx_thread_pool.h
13. ngx_cpuinfo.h
14. ngx_output_chain.c

nginx encode:
1. ngx_crc.h
2. ngx_crc32.c ngx_crc32.h
3. ngx_crypt.c ngx_crypt.h
4. ngx_md5.c ngx_md5.h
5. ngx_sha1.c ngx_sha1.h
6. ngx_murmurhash.c ngx_murmurhash.h

ngx_network
1. ngx_connection.c ngx_connection.h
2. ngx_inet.c ngx_inet.h
3. ngx_parse.c ngx_parse.h
4. ngx_parse_time.c ngx_parse_time.h
5. ngx_resolver.c ngx_resolver.h

nginx 
1. nginx.c nginx.h
2. ngx_core.h
3. ngx_conf_file.c ngx_conf_file.h
4. ngx_config.h
5. ngx_module.c ngx_module.h

nginx regex
1. ngx_regex.c ngx_regex.h
