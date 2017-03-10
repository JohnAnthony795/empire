import inspect

def prefix():
	stack = inspect.stack()
	return " " * len(stack)

def call(name):
	stack = inspect.stack()
	fun = stack[1][3]
	print "%s> %s.%s" % (" " * len(stack), name, fun)

class Node:

	def __init__(self, key_a, relation, key_b, coeff, true, false):
		self.key_a = key_a
		self.relation = relation
		self.key_b = key_b
		self.coeff = coeff
		self.true = true
		self.false = false

	def eval(self, stats):
		#p = prefix()
		#print p, "eval node: ", self.key_a, self.relation, self.coeff, self.key_b
		value_a = 0 if self.key_a not in stats else stats[self.key_a]
		value_b = 0 if self.key_b not in stats else (stats[self.key_b] * self.coeff)
		#print p, "values:", value_a, value_b
		if self.relation == "<":
			result = value_a < value_b
		elif self.relation == ">":
			result = value_a > value_b
		elif self.relation == "=":
			result = value_a == value_b
		else:
			assert False
		if result:
			#print p, "res: true"
			return self.true.eval(stats)
		else:
			#print p, "res: false"
			return self.false.eval(stats)

class Leaf:

	def __init__(self, result):
		self.result = result

	def eval(self, stats):
		#p = prefix()
		#print p, "eval leaf: ", self.result
		return self.result
