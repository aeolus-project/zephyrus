Machine parks are certain type of inital configurations, which try to simulate 
a cloud.

First, they consist of only empty locations, but there is a lot of them 
(around 100 in each of our machine parks).

Second, there are only a few categories of locations present in each 
machine park. Every location from a given "category" is exactly the same,
the only difference between them is their name (as all names must be unique).


Description of disponible machine parks.

1. Small-medium-large schema

We introduce three basic categories of locations. "Larger" a location is, more 
resources (i.e. ram) it provides, but more costly it is. However it is always
slightly more advantageous to get a bigger location, than several little ones.

Small machine provides 2048 ram, its cost is 1.
Medium machine provides 8192 ram (4x more than small), its cost is 3 (3x more).
Large machine provides 32768 ram (4x more than medium) and its cost is 11 (3,33x more).

2. Machine parks

Three basic machine parks are disponible:
100s:         100 small machines
50s-50m:      50 small and 50 medium machines
33s-33m-33l:  33 small, 33 medium and 33 large machines