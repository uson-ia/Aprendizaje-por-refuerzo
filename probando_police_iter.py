from MazeMDP import test_maze, MazeMDP
from prog_din import iteracion_politica


maze = MazeMDP(test_maze)
pi = iteracion_politica(maze, .99, .001)

print "Laberinto del fauno:"
for i in test_maze:
    print i
print "\nPolitica a seguir:\n", pi
