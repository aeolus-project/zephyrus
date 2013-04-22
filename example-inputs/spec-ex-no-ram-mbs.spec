(#@serious-wordpress = 1) 
and #(_){mbs : #MySQL > 1} = 0 
and #(_){mbs : #Wordpress > 1} = 0 
and #(_){mbs : #HTTP-proxy-load-balancer > 0 and (#MySQL > 0 or #Wordpress > 0) } = 0