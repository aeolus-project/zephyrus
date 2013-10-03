(#@glance-api > 0) and
(#@nova-api > 0) and

#(_){_ : #MySQL-backend           > 1} = 0 and
#(_){_ : #Keystone-backend        > 1} = 0 and
#(_){_ : #Glance-API-backend      > 1} = 0 and
#(_){_ : #Glance-Registry-backend > 1} = 0 and
#(_){_ : #Nova-API-backend        > 1} = 0 and
#(_){_ : #Queue-backend           > 1} = 0 and

#(_){_ : #Nova-Scheduler > 1} = 0 and
#(_){_ : #Nova-Conductor > 1} = 0 and
#(_){_ : #Nova-Compute   > 1} = 0 and

#(_){_ : #MySQL-LB           > 1} = 0 and
#(_){_ : #Keystone-LB        > 1} = 0 and
#(_){_ : #Glance-API-LB      > 1} = 0 and
#(_){_ : #Glance-Registry-LB > 1} = 0 and
#(_){_ : #Nova-API-LB        > 1} = 0 and
#(_){_ : #Queue-LB           > 1} = 0 and

true