test_maze = [[1, 2, 1, 1, 1, 0, 0, 0, 1],
             [1, 0, 0, 0, 1, 0, 1, 0, 0],
             [1, 0, 1, 1, 1, 1, 0, 0, 1],
             [0, 0, 0, 0, 3, 1, 0, 0, 1],
             [0, 1, 1, 0, 1, 1, 0, 1, 1],
             [0, 1, 0, 0, 0, 0, 0, 0, 1],
             [0, 1, 0, 1, 0, 1, 1, 0, 1],
             [1, 1, 0, 1, 0, 1, 0, 0, 1]]

test_maze.reverse()

class MazeMDP():
    def __init__(self, maze_matrix):
        self.height = len(maze_matrix)
        self.width = len(maze_matrix[0])

        self.set_states(maze_matrix)
        self.set_transitions(maze_matrix)
        self.set_actions(maze_matrix)
        self.set_rewards(maze_matrix)

    def set_states(self, maze_matrix):
        self.states = set()
        for i in range(self.height):
            for j in range(self.width):
                if maze_matrix[i][j] != 1:
                    self.states.add((j, i))

    def go(self, maze_matrix, state, action):
        x, y = state
        if action == 'up':
            x2, y2 = x, y+1
        elif action == 'down':
            x2, y2 = x, y-1
        elif action == 'left':
            x2, y2 = x-1, y
        elif action == 'right':
            x2, y2 = x+1, y
        else:
            raise("not a valid action")

        if (x2 < 0 or y2 < 0 or x2 > self.width or y2 > self.height-1 or x2 > self.width-1) or (maze_matrix[y2][x2] == 1):
            return state
        else:
            return (x2, y2)

    def turn_left(self, action):
        if action == 'up':
            return 'left'
        elif action == 'down':
            return 'right'
        elif action == 'left':
            return 'down'
        elif action == 'right':
            return 'up'
        else:
            raise("not a valid action")

    def turn_right(self, action):
        if action == 'up':
            return 'right'
        elif action == 'down':
            return 'left'
        elif action == 'left':
            return 'up'
        elif action == 'right':
            return 'down'
        else:
            raise("not a valid action")

    def set_transitions(self, maze_matrix):
        def transitions(current_state, action, next_state):
            if next_state == self.go(maze_matrix, current_state, action):
                return 0.8
            elif next_state == self.go(maze_matrix, current_state, self.turn_left(action)):
                return 0.1
            elif next_state == self.go(maze_matrix, current_state, self.turn_right(action)):
                return 0.1
            else:
                return 0.0
        self.transitions = transitions

    def set_actions(self, maze_matrix):
        self.actions = ('up', 'down', 'left', 'right')

    def set_rewards(self, maze_matrix):
        def rewards(arriving_state):
            x, y = arriving_state
            if maze_matrix[y][x] == 2:
                return 1.0
            elif maze_matrix[y][x] == 3:
                return -1.0
            else:
                return -.04
        self.r = rewards
