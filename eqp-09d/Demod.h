#ifndef TP_DEMOD_H
#define TP_DEMOD_H

/* function prototypes from demod.c */

void clear_demod_marks(Term_ptr t);

Term_ptr demodulate(Term_ptr t, Discrim_ptr demods, Gen_ptr_ptr *head_ptr);

Term_ptr demodulate_bt(Term_ptr t, void *demods, int psn, Gen_ptr_ptr *head_ptr);


int simplifiable_bt(Term_ptr t, int psn, void *demods);


#endif  /* ! TP_DEMOD_H */
