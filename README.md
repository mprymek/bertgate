BertGate
========

Kind-of-compatible BERT-rpc server and client for Elixir. 

See http://bert-rpc.org/

Features
--------
1. secure: only functions in special modules can be called
2. not secure: you can still be DDOSed by excessive atom creation 
3. (kind of) BERT-rpc compatible
4. Elixir exceptions are transparently transported to the client (this is NOT BERT-rpc compatible feature)
5. `cast` and `call` implemented
6. `info` not implemented (-> no control signals, caching etc.)

Usage
-----
Define your functions in `BertGate.Modules.YourModule`, `BertGate.Modules.OtherModule`, ...

Start server:
```
# mix do deps.get, compile, server
```
or from your application:
```
BertGate.Server.start_link
```

Test connection to the server:
```
iex(1)> conn=BertGate.Client.connect("localhost")                                         
#Port<0.1310>
iex(2)> BertGate.Client.call(conn,:'Bert',:ping,[])
:pong
```

Call your functions from client:
```
iex(2)> BertGate.Client.call(conn,:'YourModule',:my_incredible_function!,[])
```
- server will call `BertGate.Modules.YourModule.my_incredible_function!()` and return the result.

Python Interoperability
-----------------------
```
# easy_install bertrpc
# python
[...]
>>> import bertrpc
>>> service = bertrpc.Service('localhost', 9484)
>>> service.request('call').Bert.ping()
Atom('pong')
>>> service.request('call').Bert.some_integer()
1234
>>> service.request('call').Bert.some_float()
1.234
>>> service.request('call').Bert.some_atom()
Atom('this_is_atom')
>>> service.request('call').Bert.some_tuple()
(1, 2, 3, 4)
>>> service.request('call').Bert.some_bytelist()
[1, 2, 3, 4]
>>> service.request('call').Bert.some_list()
[1, 2, [3, 4]]
>>> service.request('call').Bert.some_binary()
'This is a binary'
>>> service.request('call').Bert.some_map()
{Atom('a'): 1, Atom('b'): 2}
>>> service.request('call').Bert.sum(1,6)
7
```

Exceptions are not supported by BERT-rpc - you get error on python side...
```
>>> service.request('call').Bert.exception1()
[...]
BERTRPCError: invalid response received from server
```

Async call:
```
>>> service.request('cast').Bert.ping()
```

Performance
-----------

Performance is similar to the erlang `:rpc` module:

```
# elixir --sname server -S mix server
# elixir --sname client -S mix SpeedTest server@yourdomain
**** BertGate
Range: 125 - 25341 us
Median: 162 us
Average: 209 us

**** :rpc
Range: 137 - 35067 us
Median: 177 us
Average: 219 us
```
