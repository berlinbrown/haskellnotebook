{- *********************************************************
   Method Name Map for AMQP methods by their
   according method signature.
   ********************************************************* -}
module Data.AMQP.AMQPOperations (methodNameMap) where

-- ***********************************************
{-
  Class and Method Ids:
  
  Connection    (10)
  -----------------
    Connection.Start       10
    Connection.Start_Ok    11
    Connection.Secure      20
    Connection.Secure_Ok   21
    Connection.Tune        30
    Connection.Tune_Ok     31
    Connection.Open        40
    Connection.Open_Ok     41
    Connection.Redirect    50
    Connection.Close       60
    Connection.Close_Ok    61

  Channel:     (20)
  -----------------
    Channel.Open           10
    Channel.Open_Ok        11
    Channel.Flow           20
    Channel.Flow_Ok        21
    Channel.Alert          30
    Channel.Close          40
    Channel.Close_Ok       41

  Access:      (30)
  -----------------
    Access.Request         10
    Access.Request_Ok      11

  Exchange:    (40)
  -----------------
    Exchange.Declare       10
    Exchange.Delcare_Ok    11
    Exchange.Delete        20
    Exchange.Delete_Ok     21

  Queue:       50
  -----------------
    Queue.Declare          10
    Queue.Declare_Ok       11
    Queue.Bind             20
    Queue.Bind_Ok          21
    Queue.Purge            30
    Queue.Purge_Ok         31
    Queue.Delete           40
    Queue.Delete_Ok        41

  Basic:       (60)
  -----------------
    Basic.Qos              10
    Basic.Qos_Ok           11
    Basic.Consume          20 
    Basic.Consume_Ok       21
    Basic.Cancel           30
    Basic.Cancel_Ok        31
    Basic.Publish          40
    Basic.Return           50
    Basic.Deliver          60
    Basic.Get              70
    Basic.Get_Ok           71
    Basic.Get_Empty        72
    Basic.Ack              80
    Basic.Reject           90
    
  File:        (70)
  -----------------
    File.Qos               10
    File.Qos_Ok            11
    File.Consume           20
    File.Consume_Ok        21
    File.Cancel            30
    File.Cancel_Ok         31
    File.Open              40
    File.Open_OK           41
    File.Stage             50
    File.Publish           60
    File.Return            70
    File.Deliver           80
 
  Stream:      (80)
  -----------------
    Stream.Qos             10
    Stream.Qos_Ok          11
    Stream.Consume         20
    Stream.Consume_Ok      21
    Stream.Cancel          30
    Stream.Cancel_Ok       31
    Stream.Publish         40
    Stream.Return          50
    Stream.Deliver         60

  Tx:          (90)
  -----------------
    Tx.Select              10
    Tx.Select_Ok           11
    Tx.Commit              20
    Tx.Commit_Ok           21
    Tx.Rollback            30
    Tx.Rollback_Ok         31

  Dtx:         (100)
  -----------------
    Dtx.Select             10
    Dtx.Select_Ok          11
    Dtx.Start              20
    Dtx.Start_Ok           21

  Tunnel:      (110)
  -----------------
    Tunnel.Request         10
 -}
-- ***********************************************
methodNameMap :: (Integer, Integer) -> String
methodNameMap    (10, 10) = "Connection.start"
methodNameMap    (10, 11) = "Connection.start_ok"
methodNameMap    (10, 20) = "Connection.secure"
methodNameMap    (10, 21) = "Connection.secure_ok"
methodNameMap    (10, 30) = "Connection.tune"
methodNameMap    (10, 31) = "Connection.tune_ok"
methodNameMap    (10, 40) = "Connection.open"
methodNameMap    (10, 41) = "Connection.open_ok"
methodNameMap    (10, 50) = "Connection.redirect"
methodNameMap    (10, 60) = "Connection.close"
methodNameMap    (10, 61) = "Connection.close_ok"
methodNameMap    (20, 10) = "Channel.open"
methodNameMap    (20, 11) = "Channel.open_ok"
methodNameMap    (20, 20) = "Channel.flow"
methodNameMap    (20, 21) = "Channel.flow_ok"
methodNameMap    (20, 30) = "Channel.alert"
methodNameMap    (20, 40) = "Channel.close"
methodNameMap    (20, 41) = "Channel.close_ok"
methodNameMap    (30, 10) = "Channel.access_request"
methodNameMap    (30, 11) = "Channel.access_request_ok"
methodNameMap    (40, 10) = "Channel.exchange_declare"
methodNameMap    (40, 11) = "Channel.exchange_declare_ok"
methodNameMap    (40, 20) = "Channel.exchange_delete"
methodNameMap    (40, 21) = "Channel.exchange_delete_ok"
methodNameMap    (50, 10) = "Channel.queue_declare"
methodNameMap    (50, 11) = "Channel.queue_declare_ok"
methodNameMap    (50, 20) = "Channel.queue_bind"
methodNameMap    (50, 21) = "Channel.queue_bind_ok"
methodNameMap    (50, 30) = "Channel.queue_purge"
methodNameMap    (50, 31) = "Channel.queue_purge_ok"
methodNameMap    (50, 40) = "Channel.queue_delete"
methodNameMap    (50, 41) = "Channel.queue_delete_ok"
methodNameMap    (60, 10) = "Channel.basic_qos"
methodNameMap    (60, 11) = "Channel.basic_qos_ok"
methodNameMap    (60, 20) = "Channel.basic_consume"
methodNameMap    (60, 21) = "Channel.basic_consume_ok"
methodNameMap    (60, 30) = "Channel.basic_cancel"
methodNameMap    (60, 31) = "Channel.basic_cancel_ok"
methodNameMap    (60, 40) = "Channel.basic_publish"
methodNameMap    (60, 50) = "Channel.basic_return"
methodNameMap    (60, 60) = "Channel.basic_deliver"
methodNameMap    (60, 70) = "Channel.basic_get"
methodNameMap    (60, 71) = "Channel.basic_get_ok"
methodNameMap    (60, 72) = "Channel.basic_get_empty"
methodNameMap    (60, 80) = "Channel.basic_ack"
methodNameMap    (60, 90) = "Channel.basic_reject"
methodNameMap    (60, 100) = "Channel.basic_recover"
methodNameMap    (90, 10) = "Channel.tx_select"
methodNameMap    (90, 11) = "Channel.tx_select_ok"
methodNameMap    (90, 20) = "Channel.tx_commit"
methodNameMap    (90, 21) = "Channel.tx_commit_ok"
methodNameMap    (90, 30) = "Channel.tx_rollback"
methodNameMap    (90, 31) = "Channel.tx_rollback_ok"
methodNameMap _           = "Invalid"
