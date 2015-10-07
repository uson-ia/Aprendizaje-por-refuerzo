;;; -*- coding: utf-8; mode: scheme -*-
;;; 2015 - Eduardo Acuña Yeomans
;;;
;;; Gridworld source code
;;; =====================

(import (scheme base)
	(scheme write)
	(mdp)
	(world simple-grid))

(define grid-udacity
  (grid 4 3
	'step 'step 'step 'good
	'step 'wall 'step 'hell
	'step 'step 'step 'step))

(define gridworld-udacity
  (make-simple-grid grid-udacity))

(define discount 0.5)

(define tolerance 0.01)

(define vi:utility (value-iteration gridworld-udacity
				    tolerance
				    discount))

(define vi:policy (utility->policy gridworld-udacity
				   discount
				   vi:utility))

(define pi:policy (policy-iteration gridworld-udacity
				    tolerance
				    discount))

(define pi:utility (evaluate-policy gridworld-udacity
				    discount
				    pi:policy
				    tolerance))

(display "      _   _     _            _ _               
     | | | |   | |          (_) |              
     | | | | __| | __ _  ___ _| |_ _   _       
     | | | |/ _` |/ _` |/ __| | __| | | |      
     | |_| | (_| | (_| | (__| | |_| |_| |      
      \\___/ \\__,_|\\__,_|\\___|_|\\__|\\__, |      
                                    __/ |      
                                   |___/       
 _____      _     _                    _     _ 
|  __ \\    (_)   | |                  | |   | |
| |  \\/_ __ _  __| |_      _____  _ __| | __| |
| | __| '__| |/ _` \\ \\ /\\ / / _ \\| '__| |/ _` |
| |_\\ \\ |  | | (_| |\\ V  V / (_) | |  | | (_| |
 \\____/_|  |_|\\__,_| \\_/\\_/ \\___/|_|  |_|\\__,_|
                                               \n")

(display "   ______ ______ ______ ______ ______ ______    
  |______|______|______|______|______|______|   
   ______ ______ ______ ______ ______ ______    
  |______|______|______|______|______|______|   
                                                                                              
")

(show-grid grid-udacity)

(display "\n\nSTATES\n======\n")
(display "The states are the walkable positions of the grid\n")
(display (mdp-states gridworld-udacity))

(display "\n\nTRANSITIONS\n===========\n")
(display "The probability P(next | current, action) is:\n")
(display "  0.8 if next = move(current, action)\n")
(display "  0.1 if next = move(current, turn_left(action))\n")
(display "  0.1 if next = move(current, turn_right(action))\n")
(display "  0.0 otherwise")

(display "\n\nACTIONS\n=======\n")
(display "In a given state, the legal actions are:\n")
(display "  move up\n")
(display "  move down\n")
(display "  move left\n")
(display "  move right")

(display "\n\nREWARDS\n=======\n")
(display "The reward for entering a state next comming from state current taking action move is:\n")
(display "  -1.0  if grid[next] = hell\n")
(display "  +1.0  if grid[next] = good\n")
(display "  -0.04 if grid[next] = step")

(display "\n\n#############################################\n")
(display "#############################################\n\n")
(display "            APPROXIMATING POLICIES\n")
(display "\n#############################################\n")
(display "#############################################\n\n\n")

(show-grid grid-udacity)

(display "\n\n")
(display "Value iteration\n")
(display "===============\n")
(define vi:utility (value-iteration gridworld-udacity
				    tolerance
				    discount))

(define vi:policy (utility->policy gridworld-udacity
				   discount
				   vi:utility))

(display (map (lambda (state) (list state (vi:policy state))) (mdp-states gridworld-udacity)))

(display "\n\n")
(display "Policy iteration\n")
(display "===============\n")
(define pi:policy (policy-iteration gridworld-udacity
				    tolerance
				    discount))

(define pi:utility (evaluate-policy gridworld-udacity
				    discount
				    pi:policy
				    tolerance))

(display (map (lambda (state) (list state (vi:policy state))) (mdp-states gridworld-udacity)))
(display "\n")
