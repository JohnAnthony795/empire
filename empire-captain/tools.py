import random

def debug(*args, **kwargs):
	if "level" not in kwargs or kwargs["level"] < 3:
		return
	print "DEBUG: ",
	for a in args:
		print a,
	print

warn = debug

def weighted_choice(choices):
	total = sum(w for (c, w) in choices)
	r = random.uniform(0, total)
	upto = 0
	for (c, w) in choices:
		if upto + w >= r:
			return c
		upto += w
	assert False, "shouldn't get here"
