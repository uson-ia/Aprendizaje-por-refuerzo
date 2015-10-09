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
