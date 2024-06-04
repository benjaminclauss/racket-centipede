# racket-centipede
Centipede Game for Racket

In the game Centipede, a long centipede starts at the top of the screen and zig-zags its way down to the bottom, attempting to eat the player, before making its way back to top to start all over again if both it and the player survives that long. When a centipede hits an obstacle, which is either a wall or a mushroom, it moves down (or up) a level and reverses its direction (except for obstacle avoiding, a centipede only ever moves left or right).

The player is fixed at the bottom of the screen, but can move left or right and fire a single bullet at a time (using the left/right arrow keys and space bar, respectively). Bullets are faster than the centipede (exactly how much faster is up to you, but we recommend 3–5 times as fast). If the bullet hits a mushroom, its health decrements (mushrooms take 4 hits to be killed completely and removed). If the bullet hits the centipede, the centipede splits into two at the point of impact, and a mushroom appears at the point of contact.

For more information on the centipede game, visit http://www.ccs.neu.edu/course/cs2500h/ps5.html
