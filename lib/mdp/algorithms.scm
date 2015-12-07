(define (value-iteration mdp tolerance discount)
  (define states (mdp-states mdp))
  (approximate (lambda (old-util new-util)	; good-enough?
		 (< (val-max (difference (old-util state) (new-util state))
			     for state in states)
		    tolerance))
	       (lambda (old-util)		; improve
		 (make-utility state => (bellman-utility mdp discount old-util state)
			       for state in states))
	       (lambda ()			; initial-guess
		 (make-utility state => 0.0 for state in states))))

(define (policy-iteration mdp tolerance discount)
  (define states  (mdp-states mdp))
  (define actions (mdp-actions mdp))
  (approximate (lambda (old-pi new-pi)	; good-enough?
		 (all-true? (eq? (old-pi state) (new-pi state)) for state in states))
	       (lambda (old-pi)		; improve
		 (define utility (evaluate-policy mdp discount old-pi tolerance))
		 (make-policy state => (if (> (val-max (state-action-utility mdp discount utility state action)
						      for action in (actions state))
					     (state-action-utility mdp discount utility state (old-pi state)))
					  (arg-max (state-action-utility mdp discount utility state action)
						   for action in (actions state))
					  (old-pi state))
			      for state in states))
	       (lambda ()			; initial-guess
		 (make-policy state => (choose (actions state)) for state in states))))

(define (empty-Q)
  (alist-function (dict x => 0 for x in '())))

(define (Q-ref Q key)
  (Q key))

(define (Q-set Q key val)
  (Q key val))

(define (extend visited Q state actions)
  (define (Q-init Q state actions)
    (if (null? actions)
	Q
	(let ((action (car actions)))
	  (Q-init (Q-set Q (cons state action) (* 0.1 (random-real)))
		  state
		  (cdr actions)))))
  (if (member state visited)
      (values Q visited)
      (begin
	(let ((Q (Q-init Q state actions)))
	  (values Q (cons state visited))))))

(define (choose-action Q state actions egreedy)
  (if (< (random-real) egreedy)
      (choose actions)
      (arg-max (Q-ref Q (cons state action)) for action in actions)))

(define (q-learning sim discount egreedy alpha max-epoch max-steps)
  (define (run-step step visited Q state action)
    (if (= step max-steps)
	(values Q visited)
	(let-values (((next-state reward) (sim-do sim state action)))
	  (let ((next-actions (sim-actions sim next-state)))
	    (cond ((zero? (length next-actions))
		   (let ((q-val (Q-ref Q (cons state action))))
		     (values (Q-set Q (cons state action)
				    (+ q-val (* (- reward q-val) alpha)))
			     visited)))
		  (else
		   (let-values (((Q visited) (extend visited Q next-state actions)))
		     (let ((next-action (choose-action Q next-state next-actions egreedy)))
		       (let ((q-increment (+ reward
					     (- (* discount (Q-ref Q (cons next-state next-action)))
						(Q-ref Q (cons state action))))))
			 (run-step (+ step 1)
				   visited
				   (Q-set Q (cons state action) (+ (Q-ref Q (cons state action))
								   (* alpha q-increment)))
				   next-state
				   next-action))))))))))
  (define (run-epoch epoch visited Q)
    (if (= epoch max-epoch)
	(make-policy state => (arg-max (Q-ref Q (cons state action))
				       for action in (sim-actions state))
		     for state in (map car (Q)))
	(let* ((state   (sim-initial-state sim))
	       (actions (sim-actions sim state)))
	  (let-values (((Q visited) (extend visited Q state actions)))
	    (let-values (((Q visited) (run-step 0 visited Q state (choose-action Q state actions egreedy))))
	      (run-epoch (+ epoch 1) visited Q))))))
  (run-epoch 0 '() (empty-Q)))

