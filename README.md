esnowflake
=====

esnowflake is Erlang/OTP application to generate uniq ID.  
Original design: [Twitter IDs (snowflake)](https://github.com/twitter/snowflake).

Usage
=====

```erlang
> Id = application:start(esnowflake).
ok

> esnowflake:generate_id().
896221795344384

> esnowflake:generate_ids(2).
[896498611015681,896498611015680]

> esnowflake:to_unixtime(Id).
1509193995927

> esnowflake:stats().
[{cpu_usage,[{1,0.0024724560956761723},
             {2,6.264839078604174e-5},
             {3,5.2089262927343054e-5},
             {4,6.222663816466355e-5},
             {5,0.0},
             {6,0.0},
             {7,0.0},
             {8,0.0}]},
 {total_cpu_usage,3.311773706340549e-4},
 {memory,[{total,27135728},
          {processes,7463752},
          {processes_used,7462824},
          {system,19671976},
          {atom,388625},
          {atom_used,374758},
          {binary,1259096},
          {code,8712856},
          {ets,3089264}]}]
```

Config
=====

## worker id

### application environment variable

This must be specified as not to duplicate worker ids if you use multi nodes.

- app.conf

```erlang
[
    {esnowflake, [{worker_min_max_id, [0, 9]}]}
]
```

Bench
=====

```
----------------------------------------------------
2017-10-29 16:03:27.671
b_generate_id	100000	4564.13864 ns/op


----------------------------------------------------
2017-10-29 16:03:28.703
b_generate_id	232502 op/sec

%%% esnowflake_SUITE ==> bench.b_generate_id: OK

----------------------------------------------------
2017-10-29 16:03:29.700
b_generate_ids_100	10000	96834.8342 ns/op


----------------------------------------------------
2017-10-29 16:03:30.702
b_generate_ids_100	686 op/sec

%%% esnowflake_SUITE ==> bench.b_generate_ids: OK
```
