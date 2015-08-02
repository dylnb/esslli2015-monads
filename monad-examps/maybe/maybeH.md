# Building a Sentence

* Now we can compose our safe-division computation as

                  +----------------------------------+               
                  |                                  |               
                  |                                  |               
          +-------+---------+               +--------+---------+   
          |                 |               |                  |   
          |                 |               |                  |   
       unit 2         +-----+------+     map2 (==)          unit 5   
                      |            |
                      |            |
                   map2 (+)      unit 3
                                                                  

* Smoothly evaluates to `Just True`


---


