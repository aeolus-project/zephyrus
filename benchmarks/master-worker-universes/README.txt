These universes describe a very basic master-worker architecture.

They consist of Master component type which requires a certain number
of @work ports. Then we have some Workers which always provide a certain number
of @work ports. We can have just one Worker component type or more of them.

Parameters:

- the number of @work ports required by a Master:
m10  : a Master requires 10 ports @work 
m20  : a Master requires 20 ports @work 
m40  : a Master requires 40 ports @work 
m100 : a Master requires 100 ports @work 

- the number of @work ports provided by a Worker
w1 : a Worker provides one port @work

- worker and package relations complexity:

(nothing) :
	There is only one type of Worker and a single package ("common_package")
	which implements every component type.

abpkgs :
	There are two types of Workers: Worker-A implemented by "worker_a_package"
	and Worker-B implemented by "worker_b_package". These two packages are in
	conflict. Two types of Workers are exactly identical apart from that.
	This generates a lot of symmetrical solutions which clearly would not be
	easy to ger rid of.