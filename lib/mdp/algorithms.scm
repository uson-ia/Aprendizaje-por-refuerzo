(define (value-iteration mdp tolerance discount)
  (define states (mdp-states mdp))
  (approximate (lambda (utility utility*)	; good-enough?
		 (< (val-max (difference (utility state) (utility* state))
			     for state in states)
		    tolerance))
	       (lambda (utility)		; improve
		 (make-utility state => (bellman-utility mdp discount utility state)
			       for state in states))
	       (lambda ()			; initial-guess
		 (make-utility state => 0.0 for state in states))))

(define (policy-iteration mdp tolerance discount)
  (define states  (mdp-states mdp))
  (define actions (mdp-actions mdp))
  (approximate (lambda (policy:utility policy*:utility*) ; good-enough?
		 (define utility (cdr policy:utility))
		 (define utility* (cdr policy*:utility*))
		 (< (val-max (difference (utility state) (utility* state)) for state in states)
		    (/ tolerance (length states))))
	       (lambda (policy:utility)	 ; improve
		 (define policy (car policy:utility))
		 (define utility (cdr policy:utility))
		 (define policy*
		   (make-policy state => (if (> (val-max (state-action-utility mdp discount utility state action)
							for action in (actions state))
					       (state-action-utility mdp discount utility state (policy state)))
					    (arg-max (state-action-utility mdp discount utility state action)
						     for action in (actions state))
					    (policy state))
				for state in states))
		 (cons policy (evaluate-policy mdp discount policy)))
	       (lambda ()
		 (define policy (make-policy state => (choose (actions state)) for state in states))
		 (cons policy (evaluate-policy mdp discount policy)))))
