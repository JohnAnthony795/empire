import handler
import situation
import parser
import algos

# This class trusts messages and the state describes in situation (no assert in this class).
class Continent(handler.Handler):

	def __init__(self, the_situation):
		self.the_situation = the_situation
		self.groups = {}
		self.terrains = {}
		self.links = {}
		self.frontiers = {}
		self.counter = 0

	def show(self):
		print "GROUPS:"
		for r in range(self.height):
			print " " * r,
			for q in range(self.width):
				if self.view[q][r] is None:
					print "  ",
				else:
					print "%2d" % self.view[q][r],
			print
		for group in self.groups:
			terrain = self.terrains[group]
			print "%s %d:" % ("WATER" if terrain == self.the_situation.WATER else "GROUND", group)
			i = 0
			for location in self.groups[group]:
				s = "(%d,%d)" % location
				s = "%s%s" % (" " * (9 - len(s)), s)
				if i % 8 == 0 and i > 0:
					print ""
					print "  ",
				i = i + 1
				print s,
			print ""
		print "LINKS:"
		for link in self.links:
			print " ", link, self.links[link]

	def end(self):
		self.show()

	def set_height(self, height):
		self.height = height
		if hasattr(self, "width"):
			self.view = [[None for r in range(self.height)] for q in range(self.width)] 

	def set_width(self, width):
		self.width = width
		if hasattr(self, "height"):
			self.view = [[None for r in range(self.height)] for q in range(self.width)] 

	def get_group(self, location):
		q, r = location
		return self.view[q][r]

	def get_neighbors(self, group):
		return self.links[group]

	def get_locations(self, group):
		return self.groups[group]

	def update(self):
		# Update list of unexplored tile along the group, for each group and
		# the list of unexplored tile inside the group, for each group.
		# TODO
		# TODO
		# TODO
		pass

	def helper(self, location, terrain):
		q, r = location
		if self.view[q][r] is None:
			# Get neighbors associated to the same terrain type
			# and get the neighbors with different terrain type (for links).
			same = []
			link = []
			for (qd, rd) in algos.directions:
				x = (qd + q, rd + r)
				if self.the_situation.is_in_map(x):
					# In the following, 'n' suffix stands for neighbors,
					# 't' prefix for terrain and 'n' prefix for number (identifier).
					qn, rn = x
					if self.view[qn][rn] is not None:
						nn = self.view[qn][rn]
						if self.terrains[nn] == terrain:
							if nn not in same:
								same.append(nn)
						else:
							if nn not in link:
								link.append(nn)
			# If there are no neighbor of the same terrain, create a new ocean or
			# continent. If only one neighbor of the same type, then add the location
			# to the corresponding ocean or continent. Otherwise, there are multiple
			# neighbor of the same type (this location is a bridge) and we must merge
			# the corresponding oceans or continents.
			if len(same) == 0:
				# Create a new group and links.
				n = self.counter
				self.counter = self.counter + 1
				self.groups[n] = [location]
				self.terrains[n] = terrain
				self.links[n] = link
				for x in link:
					self.links[x].append(n)
			elif len(same) == 1:
				# Add to the groupd and update links if needed.
				n = same[0]
				self.groups[n].append(location)
				for x in link:
					if x not in self.links[n]:
						self.links[n].append(x)
					if n not in self.links[x]:
						self.links[x].append(n)
			else:
				# Use one of the group as the representative, merge the other, update links
				# and update the view.
				n = same[0]
				locations = [x for i in same for x in self.groups[i]]
				locations.append(location)
				links = list(set([x for i in same for x in self.links[i]]))
				self.groups[n] = locations
				self.links[n] = links
				for x in links:
					for i in same:
						if i in self.links[x]:
							self.links[x].remove(i)
					self.links[x].append(n)
				for x in same:
					if x != n:
						del self.links[x]
						del self.groups[x]
				for (qg, rg) in locations:
					self.view[qg][rg] = n
			self.view[q][r] = n

	def set_visible_none(self, location, terrain):
		self.helper(location, terrain)

	def set_visible_owned_city(self, location, terrain, city_id, owner):
		self.helper(location, terrain)

	def set_visible_city(self, location, terrain, city_id):
		self.helper(location, terrain)

	def set_visible_piece(self, location, terrain, owner, piece_id, piece_type_id, piece_hits):
		self.helper(location, terrain)
