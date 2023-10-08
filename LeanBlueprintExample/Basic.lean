import Mathlib.Tactic

def one := 1

def two := 2

theorem one_plus_one_eq_two : one + one = two := by
  rfl

def one_plus_one_eq_two_with_def : Prop := one + one = two
