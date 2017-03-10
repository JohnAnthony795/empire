import algos

class Stats:

	# Parameters for city production decision.
	# First, the following parameters are computed for each group:
	#           g: value 1
	#           n: number of neighbor groups
	#          pp: number of player pieces in the group
	#          ep: number of enemy pieces in the group
	#          pc: number of player cities in the group
	#          ec: number of enemy cities in the group
	#          fc: number of free cities in the group
	#         p@2: number of player pieces of type 2 in the group
	#         e@2: number of enemy pieces of type 2 in the group
	#    p@2.full: number of player pieces of type 2 full in the group
	#    e@2.full: number of enemy pieces of type 2 full in the group
	# Then, the parameters 'N*', where '*' is replaced by any previous key, are
	#   computed for each group. They correspond to the addition of the previous values
	#   for all neighbors of the group (hence, we need previous parameters for each group first)
	# The parameters 'Cw*', where '*' is replaced by any previous key, are computed
	#   for each player city. They correspond to the addition of the previous values
	#   for all water groups reachables from the city.
	# The parameters 'Cg*', where '*' is replaced by any previous key, are computed
	#   for each player city. They correspond to the addition of the previous values
	#   for all ground groups reachables from the city.
	# The parameters 'nCw' and 'nCg' indicates the number of water neighbor groups and
	#   ground neighbour groups. They include the group where the city stands on.

	def __init__(self, the_situation, the_continent):
		self.the_situation = the_situation
		self.the_continent = the_continent

	def update_piece(self, piece, owner):
		location = piece.get_location()
		group = self.the_continent.get_group(location)
		piece_id = piece.piece_id
		piece_type_id = piece.piece_type_id
		piece_type = self.the_situation.piece_types[piece_type_id]
		# Update stats.
		self.group_stats[group]["%sp" % owner] += 1
		self.group_stats[group]["%s@%d" % (owner, piece_type_id)] += 1
		if owner == "p" and len(piece.transport) == piece_type.capacity:
			self.group_stats[group]["%s@%d.full" % (owner, piece_type_id)] += 1

	def update_city(self, city, owner):
		location = city.get_location()
		group = self.the_continent.get_group(location)
		city_id = city.city_id
		# Update stats.
		self.group_stats[group]["%sc" % owner] += 1

	def refine_group(self, group):
		# Get neighbors of the group.
		neighbors = self.the_continent.get_neighbors(group)
		# Create the stats for this city by merging stats from identified groups.
		stats = {}
		for neighbor in neighbors:
			neighbor_stats = self.group_stats[neighbor]
			for key in neighbor_stats:
				if key[0] != 'N':
					new_key = "N" + key
					if new_key not in stats:
						stats[new_key] = 0
					stats[new_key] += neighbor_stats[key]
		# !We must extend the dictionary and not replace this dictionary!
		self.group_stats[group].update(stats)
		self.group_stats[group]["n"] = len(neighbors)

	def refine_city(self, city):
		# Identify waters accessibles from the city.
		location = city.get_location()
		group = self.the_continent.get_group(location)
		waters = set([])
		grounds = set([])
		for direction in algos.directions:
			neighbor_location = algos.add_locations(direction, location)
			if self.the_situation.is_in_map(neighbor_location):
				if self.the_situation.get_terrain(neighbor_location) == self.the_situation.WATER:
					waters.add(self.the_continent.get_group(neighbor_location))
				else:
					grounds.add(self.the_continent.get_group(neighbor_location))
		waters = list(waters)
		grounds = list(grounds)
		# Create the stats for this city by adding stats of water groups and stats of ground groups.
		# If the city is not near water, the key 'Cw*' will not be present.
		stats = {}
		for water in waters:
			water_stats = self.group_stats[water]
			for key in water_stats:
				new_key = "Cw" + key
				if new_key not in stats:
					stats[new_key] = 0
				stats[new_key] += water_stats[key]
		for ground in grounds:
			ground_stats = self.group_stats[ground]
			for key in ground_stats:
				new_key = "Cg" + key
				if new_key not in stats:
					stats[new_key] = 0
				stats[new_key] += ground_stats[key]
		stats["nCw"] = len(waters)
		stats["nCg"] = len(grounds)
		self.city_stats[city.city_id] = stats

	def update(self):
		self.group_stats = {}
		self.city_stats = {}

		for group in self.the_continent.groups:
			stats = {"g":1, "pp":0, "ep":0, "pc":0, "ec":0, "fc":0}
			for piece_type_id in self.the_situation.piece_types:
				stats["p@%d" % piece_type_id] = 0
				stats["e@%d" % piece_type_id] = 0
				stats["p@%d.full" % piece_type_id] = 0
				stats["e@%d.full" % piece_type_id] = 0
			self.group_stats[group] = stats

		for piece in self.the_situation.get_player_pieces():
			self.update_piece(piece, "p")
		for piece in self.the_situation.get_enemy_pieces():
			self.update_piece(piece, "e")
		for city in self.the_situation.get_player_cities():
			self.update_city(city, "p")
		for city in self.the_situation.get_enemy_cities():
			self.update_city(city, "e")
		for city in self.the_situation.get_free_cities():
			self.update_city(city, "f")

		for group in self.the_continent.groups:
			self.refine_group(group)
		for city in self.the_situation.get_player_cities():
			self.refine_city(city)

	def show(self):
		print "STATS:"
		for group in self.group_stats:
			print "  group: %d" % group
			stats = self.group_stats[group]
			for k in stats:
				print "    %s: %d" % (k, stats[k])
		for city_id in self.city_stats:
			print "  city_id: %d" % city_id
			stats = self.city_stats[city_id]
			for k in stats:
				print "    %s: %d" % (k, stats[k])
