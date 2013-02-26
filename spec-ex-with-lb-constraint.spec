(#@serious-wordpress >= 1) 
and #(_){debian-squeeze : #MySQL > 1} = 0 
and #(_){debian-squeeze : #Wordpress > 1} = 0 
and #(_){debian-squeeze : #HTTP-proxy-load-balancer > 0 and (#MySQL > 0 or #Wordpress > 0) } = 0