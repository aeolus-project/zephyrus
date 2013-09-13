(#@glance-api > 0) and
(#@nova-api > 0) and

#(_){_ : #Plugin-keystone-data-backend-to-mysql > 1 and #Keystone-backend = 0} = 0 and

#(_){_ : #MySQL-LB      > 0 and #LoadBalancer = 0} = 0 and
#(_){_ : #Keystone-LB   > 0 and #LoadBalancer = 0} = 0 and
#(_){_ : #Glance-API-LB > 0 and #LoadBalancer = 0} = 0 and
#(_){_ : #Nova-API-LB   > 0 and #LoadBalancer = 0} = 0