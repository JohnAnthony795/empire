import math

import algos
import parameters

class Influence:

	def __init__(self, the_situation, the_units, the_parameters):
		self.the_situation = the_situation
		self.the_units = the_units
		self.the_parameters = the_parameters

	def update(self):
		the_situation = self.the_situation
		self.height = the_situation.height
		self.width = the_situation.width

		self.influence_player_cities = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_player_pieces = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_free_cities = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_enemies_cities = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_enemies_pieces = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_guessed_cities = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_guessed_enemies_pieces = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_guessed_enemies_cities = [[0 for x in range(self.height)] for x in range(self.width)]
		self.influence_unexplored = [[0 for x in range(self.height)] for x in range(self.width)]

		for q in range(self.width):
			for r in range(self.height):
				location = q, r
				tile = self.the_situation.get_tile(location)
				cell = self.the_units.get(location)
				if tile.terrain is not None:
					adjacent_unexplored = False
					for (qd, rd) in algos.directions:
						neighbor_location = q + qd, r + rd
						if self.the_situation.is_in_map(neighbor_location) and self.the_situation.get_tile(neighbor_location).terrain is None:
							adjacent_unexplored = True
							break
					if adjacent_unexplored:
						influence_map = self.influence_unexplored
						self.expand_influence(influence_map, location, \
							self.the_parameters.influence_unexplored_intensity, \
							self.the_parameters.influence_unexplored_step_length, \
							self.the_parameters.influence_unexplored_nb_steps, \
							[self.the_situation.get_tile(location).terrain])
				if cell is None:
					pass
				elif cell.type == self.the_units.CITY:
					influence_map = None
					if cell.visible:
						influence_map = self.influence_free_cities
					else:
						influence_map = self.influence_guessed_cities
					self.expand_influence(influence_map, location, \
						self.the_parameters.influence_city_intensity, \
						self.the_parameters.influence_city_step_length, \
						self.the_parameters.influence_city_nb_steps, \
						the_situation.terrains)
				elif cell.type == self.the_units.OWNED_CITY:
					influence_map = None
					if cell.visible:
						if cell.owner == the_situation.player_id:
							influence_map = self.influence_player_cities
						else:
							influence_map = self.influence_enemies_cities
					else:
						# There is no reason that the city of the enemy is lost because the player is the only one able
						# to take it, but he does not see this city. So, this city still belongs to the enemy.
						influence_map = self.influence_guessed_enemies_cities
					self.expand_influence(influence_map, location, \
						self.the_parameters.influence_city_intensity, \
						self.the_parameters.influence_city_step_length, \
						self.the_parameters.influence_city_nb_steps, \
						the_situation.terrains)
				elif cell.type == self.the_units.PIECE:
					influence_map = None
					if cell.visible:
						ratio = 1.0
						if cell.owner == the_situation.player_id:
							influence_map = self.influence_player_pieces
						else:
							influence_map = self.influence_enemies_pieces
					else:
						# The intensity depends on the freshness of the information.
						ratio = 1.0 / math.log(10.0 + (self.the_units.date - cell.date) * 10, 10)
						influence_map = self.influence_guessed_enemies_pieces
					piece_type = the_situation.piece_types[cell.piece_type_id]
					self.expand_influence(influence_map, location, \
						piece_type.strength * piece_type.max_hits * self.the_parameters.influence_piece_intensity_factor * ratio, \
						#piece_type.strength * piece_type.max_hits * self.the_parameters.influence_piece_intensity_factor * ratio, \
						###self.the_parameters.influence_piece_intensity_factor * ratio, \
						piece_type.speed * self.the_parameters.influence_piece_step_length_factor, \
						self.the_parameters.influence_piece_nb_steps, \
						piece_type.terrains)

	def expand_influence(self, influence_map, location, intensity, step_length, nb_steps, terrains):
		situation = self.the_situation
		depth = step_length * nb_steps
		results = algos.breadth_first_terrains(location, depth, situation, terrains)
		reachable, came_from, path_len = results
		reachable.append(location)
		path_len[location] = 0
		for (q, r) in reachable:
			influence_map[q][r] += intensity * math.exp(-float(path_len[(q,r)]) / step_length)

	def get_influence_player_cities(self, location):
		q, r = location
		return self.influence_player_cities[q][r]

	def get_influence_player_pieces(self, location):
		q, r = location
		return self.influence_player_pieces[q][r]

	def get_influence_enemies_cities(self, location):
		q, r = location
		return self.influence_enemies_cities[q][r]

	def get_influence_enemies_pieces(self, location):
		q, r = location
		return self.influence_enemies_pieces[q][r]

	def get_influence_guessed_enemies_pieces(self, location):
		q, r = location
		return self.influence_guessed_enemies_pieces[q][r]

	def get_influence_guessed_enemies_cities(self, location):
		q, r = location
		return self.influence_guessed_enemies_cities[q][r]

	def get_influence_free_cities(self, location):
		q, r = location
		return self.influence_free_cities[q][r]

	def get_influence_guessed_cities(self, location):
		q, r = location
		return self.influence_guessed_cities[q][r]

	def get_influence_unexplored(self, location):
		q, r = location
		return self.influence_unexplored[q][r]

	def show(self):
		print "INFLUENCE PLAYER CITIES:"
		self.show_influence_map(self.influence_player_cities)
		print "INFLUENCE PLAYER PIECES:"
		self.show_influence_map(self.influence_player_pieces)
		print "INFLUENCE FREE CITIES:"
		self.show_influence_map(self.influence_free_cities)
		print "INFLUENCE ENEMIES CITIES:"
		self.show_influence_map(self.influence_enemies_cities)
		print "INFLUENCE ENEMIES PIECES:"
		self.show_influence_map(self.influence_enemies_pieces)
		print "INFLUENCE GUESSED CITIES:"
		self.show_influence_map(self.influence_guessed_cities)
		print "INFLUENCE GUESSED ENEMIES PIECES:"
		self.show_influence_map(self.influence_guessed_enemies_pieces)
		print "INFLUENCE GUESSED ENEMIES CITIES:"
		self.show_influence_map(self.influence_guessed_enemies_cities)

	def show_influence_map(self, influence_map):
		for j in range(self.height):
			line = " " * j + "\\"
			for i in range(self.width):
				if influence_map[i][j] != 0:
					n = "%.2f " % influence_map[i][j]
					p = " " * (8 - len(n))
					line = line + ("%s%s" % (p, n))
				else:
					line = line + (" " * 8)
			print line
