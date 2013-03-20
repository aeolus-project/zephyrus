(#@serious-wordpress = 1) 
and #(_){_ : #MySQL > 1} = 0 
and #(_){_ : #Wordpress > 1} = 0 
and #(_){_ : #HTTP-proxy-load-balancer > 0 and (#MySQL > 0 or #Wordpress > 0) } = 0