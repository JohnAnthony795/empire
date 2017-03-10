import algos
import handler
import tools

class Tile:
	def __init__(self, terrain, location):
		self.terrain = terrain	# If None: unexplored.
		self.content = None
		self.visible = False
		self.location = location
		self.parent = None
	def get_location(self):
		return self.location
	def is_city(self):
		return False
	def is_owned_city(self):
		return False

class Piece:
	# States used for transporters.
	TRAVEL_TAKE=0
	TRAVEL_LAND=1
	TAKE=2
	LAND=3
	def __init__(self, piece_id, piece_type_id, owner, piece_hits):
		self.piece_hits = piece_hits
		self.piece_id = piece_id
		self.piece_type_id = piece_type_id
		self.owner = owner
		# TODO: interet ????
		#######self.content = []
		self.transport = []
		self.parent = None
		# Following attributes are not used by situation but
		# they are used by other classes.
		self.behavior = None
		self.steps_left = None
		self.path = None
		self.destination = None
		self.role = None
	def get_location(self):
		return self.parent.get_location()
	def is_piece(self):
		return True
	def is_city(self):
		return False
	def is_owned_city(self):
		return False

class OwnedCity:
	def __init__(self, city_id, parent, owner):
		self.city_id = city_id
		self.owner = owner
		self.parent = parent
		self.production = None
		self.content = []
	def get_location(self):
		return self.parent.get_location()
	def is_piece(self):
		return False
	def is_city(self):
		return True
	def is_owned_city(self):
		return True

class City:
	def __init__(self, city_id, parent):
		self.city_id = city_id
		self.parent = parent
	def get_location(self):
		return self.parent.get_location()
	def is_piece(self):
		return False
	def is_city(self):
		return True
	def is_owned_city(self):
		return False

class PieceType:
	def __init__(self, dictionary):
		for k, v in dictionary.items():
			setattr(self, k, v)

class Situation(handler.Handler):

	GROUND=0
	WATER=1

	def __init__(self):
		self.player_cities = {}
		self.player_pieces = {}
		self.enemy_cities = {}
		self.enemy_pieces = {}
		self.free_cities = {}
		self.terrains = [self.GROUND, self.WATER]

	def get_free_cities(self):
		return self.free_cities.values()

	def get_player_pieces(self):
		return self.player_pieces.values()

	def get_player_cities(self):
		return self.player_cities.values()

	def get_enemy_pieces(self):
		return self.enemy_pieces.values()

	def get_enemy_cities(self):
		return self.enemy_cities.values()

	def check(self):
		err = False
		# Check enemy pieces.
		for piece_id in self.enemy_pieces:
			piece = self.enemy_pieces[piece_id]
			if piece.owner == self.player_id:
				tools.warn("enemy piece_id %d belongs to player" % piece_id)
				err = True
			if piece_id in self.player_pieces:
				tools.warn("enemy piece_id %d also in player_pieces" % piece_id)
				err = True
			parent = piece.parent
			if isinstance(parent, Tile):
				if piece != parent.content:
					q, r = parent.get_location()
					tools.warn("enemy piece_id %d invalid parent %d %d %s" % (piece_id, q, r, str(parent)))
					err = True
		# Check player pieces.
		for piece_id in self.player_pieces:
			piece = self.player_pieces[piece_id]
			if piece.owner != self.player_id:
				tools.warn("player piece_id %d belongs to enemy" % piece_id)
				err = True
			# Test if piece_id also in enemy_pieces done in the previous loop.
			parent = piece.parent
			if isinstance(parent, Tile):
				if piece != parent.content:
					q, r = parent.get_location()
					tools.warn("enemy piece_id %d invalid parent %d %d %s" % (piece_id, q, r, str(parent)))
					err = True
		# Check free cities.
		for city_id in self.free_cities:
			city = self.free_cities[city_id]
			parent = city.parent
			if parent is None or not isinstance(parent, Tile):
				tools.warn("free city_id %d parent is not a tile %s" % (city_id, str(parent)))
				err = True
			if city_id in self.enemy_cities or city_id in self.player_cities:
				tools.warn("free city_id %d also in enemy_cities or player_cities" % city_id)
				err = True
		# Check player cities.
		for city_id in self.player_cities:
			city = self.player_cities[city_id]
			parent = city.parent
			if parent is None or not isinstance(parent, Tile):
				tools.warn("player city_id %d parent is not a tile %s" % (city_id, str(parent)))
				err = True
			if city_id in self.enemy_cities or city_id in self.free_cities:
				tools.warn("player city_id %d also in enemy_cities or player_cities" % city_id)
				err = True
		# Check enemy cities.
		for city_id in self.enemy_cities:
			city = self.enemy_cities[city_id]
			parent = city.parent
			if parent is None or not isinstance(parent, Tile):
				tools.warn("enemy city_id %d parent is not a tile %s" % (city_id, str(parent)))
				err = True
			if city_id in self.player_cities or city_id in self.free_cities:
				tools.warn("enemy city_id %d also in enemy_cities or player_cities" % city_id)
				err = True
		# Check the view.
		for q in range(self.width):
			for r in range(self.height):
				tile = self.view[q][r]
				content = tile.content
				if content is None:
					pass
				elif content.parent != tile:
					message = "element %s at %d %d is not the tile itself"
					tools.warn(message % (str(content), q, r))
					err = True
				elif isinstance(content, Piece):
					piece_id = content.piece_id
					if piece_id not in self.player_pieces and piece_id not in self.enemy_pieces:
						message = "piece_id %d at %d %d is neither in player_pieces nor in enemy_pieces"
						tools.warn(message % (piece_id, q, r))
						err = True
				elif isinstance(content, City):
					city_id = content.city_id
					if city_id not in self.free_cities:
						message = "free city_id %d at %d %d is not in free_cities"
						tools.warn(message % (city_id, q, r))
						err = True
				elif isinstance(content, OwnedCity):
					city_id = content.city_id
					if city_id not in self.enemy_cities and city_id not in self.player_cities:
						message = "owned city_id %d at %d %d is not in enemy_cities nor in player_cities"
						tools.warn(message % (city_id, q, r))
						err = True
				else:
					tools.warn("unknown content %s" % str(content))
					err = True
		if err:
			raise Exception("integrity error")

	# SET CONFIGURATION

	# The configuration is associated to class attributes created on the fly
	# (view, width, height, player_id, piece_types).
	# They are defined while reading the first messages sent by the server.
	# This way, an attempt to read a configuration not already set will trigger an
	# exception.

	def set_height(self, height):
		#assert not hasattr(self, "height")
		self.height = height
		if hasattr(self, "width"):
			self.view = [[Tile(None, (q, r)) for r in range(self.height)] for q in range(self.width)] 

	def set_width(self, width):
		#assert not hasattr(self, "width")
		self.width = width
		if hasattr(self, "height"):
			self.view = [[Tile(None, (q, r)) for r in range(self.height)] for q in range(self.width)] 

	def set_player_id(self, player_id):
		#assert not hasattr(self, "player_id")
		self.player_id = player_id

	def set_piece_types(self, piece_types):
		#assert not hasattr(self, "piece_types")
		self.piece_types = piece_types

	# SHOW VIEW

	def show(self):
		if not hasattr(self, "width") or not hasattr(self, "height") or not hasattr(self, "player_id"):
			print "width or height or player_id not set"
		else:
			print "width:%d height:%d player_id:%d" % (self.width, self.height, self.player_id)
			for r in range(self.height):
				line = "  " * r + "\\"
				for q in range(self.width):
					tile = self.view[q][r]
					if not tile.visible and tile.terrain is None:
						line = line + "  "
					elif not tile.visible:
						if tile.terrain == self.WATER:
							line = line + ". "
						elif tile.terrain == self.GROUND:
							line = line + "+ "
						else:
							#assert False
							pass
					elif tile.content == None:
						if tile.terrain == self.WATER:
							line = line + ". "
						elif tile.terrain == self.GROUND:
							line = line + "+ "
						else:
							#assert False
							pass
					else:
						content = tile.content
						if isinstance(content, Piece):
							piece_symbol = self.piece_types[content.piece_type_id].symbol
							line = line + piece_symbol + ("%d" % content.owner)
						elif isinstance(content, City):
							line = line + "O "
						elif isinstance(content, OwnedCity):
							line = line + ("O%d" % content.owner)
						else:
							#assert False
							pass
				print line

	# GET INFORMATION ON SITUATION

	def get_tile(self, location):
		#assert self.is_in_map(location)
		q, r = location
		return self.view[q][r]

	def get_content(self, location):
		return self.get_tile(location).content

	def get_terrain(self, location):
		return self.get_tile(location).terrain

	def get_player_piece(self, piece_id):
		#assert piece_id in self.player_pieces
		return self.player_pieces[piece_id]

	def get_enemy_piece(self, piece_id):
		#assert piece_id in self.enemy_pieces
		return self.enemy_pieces[piece_id]

	def get_player_city(self, city_id):
		#assert city_id in self.player_cities
		return self.player_cities[city_id]

	def is_in_map(self, location):
		q, r = location
		return 0 <= q and q < self.width and 0 <= r and r < self.height

	def can_player_piece_be_on(self, piece, destination):
		# Get the piece type.
		piece_type = self.piece_types[piece.piece_type_id]
		# Can't move outside the map.
		if not self.is_in_map(destination):
			return False
		tile = self.get_tile(destination)
		# Can be in an unexplored tile.
		if tile.terrain is None:
			return True
		# Can go in an empty tile with an adequate terrain.
		content = tile.content
		if content == None:
			return tile.terrain in piece_type.terrains
		# Can go in a city if it can invade or if it is player's city.
		if isinstance(content, City):
			return piece_type.can_invade
		if isinstance(content, OwnedCity):
			return content.owner == self.player_id or piece_type.can_invade
		# Can attack with an adequate terrain.
		if content.owner != self.player_id:
			return tile.terrain in piece_type.terrains
		# Can also be transported if the transport has enough room.
		other_piece_type = self.piece_types[content.piece_type_id]
		return piece_type.piece_type_id in other_piece_type.transportable and len(content.transport) < other_piece_type.capacity

	def can_player_piece_safely_be_on(self, piece, destination):
		piece_type = self.piece_types[piece.piece_type_id]
		# Can't move outside the map.
		if not self.is_in_map(destination):
			return False
		tile = self.get_tile(destination)
		# The piece can go 1) in an unexplored tile or
		# 2) in an empty tile with an adequate terrain or 3) in an owned city.
		content = tile.content
		return tile.terrain is None or \
				(content == None and tile.terrain in piece_type.terrains) or \
				(isinstance(content, OwnedCity) and content.owner == self.player_id)
		
#	def is_valid_path(self, piece, path):
#		if path[0] != piece.get_location():
#			return False
#		for i in range(1, len(path)):
#			if not (self.can_player_piece_be_on(piece, path[i]) and \
#					algos.are_locations_adjacents(path[i - 1], path[i])):
#				return False
#		return True

	def is_player_piece(self, piece_id):
		return piece_id in self.player_pieces

	def is_tile_none(self, location):
		return self.get_content(location) is None

	def is_tile_player_piece(self, location):
		content = self.get_content(location)
		return isinstance(content, Piece) and content.owner == self.player_id

	def is_tile_player_city(self, location):
		content = self.get_content(location)
		return isinstance(content, OwnedCity) and content.owner == self.player_id

	def is_tile_enemy_city(self, location):
		content = self.get_content(location)
		return isinstance(content, OwnedCity) and content.owner != self.player_id

	def is_tile_free_city(self, location):
		content = self.get_content(location)
		return isinstance(content, City)

	def is_tile_enemy_piece(self, location):
		content = self.get_content(location)
		return isinstance(content, Piece) and content.owner != self.player_id

	# CHANGES ON SITUATION

	def set_visible_none(self, location, terrain):
		# The previous content can't be:
		# - a city: a city can't move;
		# - an owned city: an owned city can't move.
		# - a piece of the player: the piece first leave the terrain.
		# The previous content can be:
		# - nothing;
		# - a piece of the enemy.
		tile = self.get_tile(location)
		if tile.content is not None:
			#assert isinstance(tile.content, Piece)
			piece_id = tile.content.piece_id
			#assert piece_id not in self.player_pieces
			#assert piece_id in self.enemy_pieces
			del self.enemy_pieces[piece_id]
		tile.visible = True
		tile.content = None
		tile.terrain = terrain

	def set_visible_owned_city(self, location, terrain, city_id, owner):
		tile = self.get_tile(location)
		tile.visible = True
		tile.terrain = terrain
		if tile.content is None:
			city = OwnedCity(city_id, tile, owner)
			tile.content = city
			if owner == self.player_id:
				self.player_cities[city_id] = city
			else:
				self.enemy_cities[city_id] = city
		elif isinstance(tile.content, City):
			# If the previous element was a City, then it was previously
			# defined in dictionnary (free_cities). It must be removed from
			# this dictionnary.
			#assert city_id in self.free_cities
			del self.free_cities[city_id]
			city = OwnedCity(city_id, tile, owner)
			tile.content = city
			if owner == self.player_id:
				self.player_cities[city_id] = city
			else:
				self.enemy_cities[city_id] = city
		else:
			#assert isinstance(tile.content, OwnedCity)
			if owner == self.player_id:
				# The owned city now belongs to the player.
				# If it already belongs to the player, do nothing.
				# Otherwise, it must be removed from enemy_cities and recreated
				# as a city owned by the player.
				if tile.content.owner == owner:
					#assert city_id in self.player_cities
					pass
				else:
					#assert city_id in self.enemy_cities
					del self.enemy_cities[city_id]
					city = OwnedCity(city_id, tile, owner)
					tile.content = city
					self.player_cities[city_id] = city
			else:
				# The owned city now belongs to the enemy. Remove corresponding
				# reference in dictionnaries and create a new owned city.
				if city_id in self.enemy_cities:
					del self.enemy_cities[city_id]
				if city_id in self.player_cities:
					del self.player_cities[city_id]
				city = OwnedCity(city_id, tile, owner)
				tile.content = city
				if owner == self.player_id:
					self.player_cities[city_id] = city
				else:
					self.enemy_cities[city_id] = city

	def set_visible_city(self, location, terrain, city_id):
		# Keep it simple: recreate the city and reset the tile.
		tile = self.get_tile(location)
		tile.visible = True
		tile.terrain = terrain
		#assert not isinstance(tile.content, Piece)
		#assert not isinstance(tile.content, OwnedCity)
		city = City(city_id, tile)
		self.free_cities[city_id] = city
		tile.content = city

	def set_visible_piece(self, location, terrain, owner, piece_id, piece_type_id, piece_hits):
		tile = self.get_tile(location)
		tile.terrain = terrain
		# Maybe there is already something in the tile. Either it is the piece itself, or
		# it must be an enemy piece which can be deleted (killed by the player piece).
		# It can't be another player piece (a delete_piece precedes the set_visible event).
		if tile.content != None:
			#assert isinstance(tile.content, Piece)
			tile.content.parent = None
			if tile.content.owner != self.player_id:
				del self.enemy_pieces[tile.content.piece_id]
			else:
				#assert piece_id == tile.content.piece_id
				pass
		# Retrieve the existing player piece or create a new one for other.
		if owner == self.player_id:
			# Must have been inserted while processing create_piece event.
			# We must update the piece_hits.
			#assert piece_id in self.player_pieces
			piece = self.player_pieces[piece_id]
			piece.piece_hits = piece_hits
		else:
			piece = Piece(piece_id, piece_type_id, owner, piece_hits)
			self.enemy_pieces[piece_id] = piece
		piece.parent = tile
		tile.visible = True
		tile.content = piece

	def set_explored(self, location, terrain):
		# XXX: the tile is now not-visible. Its content was either:
		#   1. a player piece which moved in other tile,
		#   2. or a player piece which is dead,
		#   3. or an piece of another player.
		# For 1., it's ok, the piece will appear in other tile. For 2., a
		# delete_piece will come. But, for 3., we must wait the end of the
		# turn and flush the arrays for not-visible pieces and cities.
		tile = self.get_tile(location)
		content = tile.content
		if content != None:
			content.parent = None
			if isinstance(content, City):
				del self.free_cities[content.city_id]
			elif isinstance(content, OwnedCity):
				# A lose_city must arrive before the corresponding set_explored.
				#assert content.owner != self.player_id
				#assert content.city_id in self.enemy_cities
				del self.enemy_cities[content.city_id]
			else:
				#assert isinstance(content, Piece)
				# A delete_piece arrive before the corresponding set_explored.
				#assert content.owner != self.player_id
				#assert content.piece_id in self.enemy_pieces
				del self.enemy_pieces[content.piece_id]
		tile.content = None
		tile.visible = False

	def create_piece(self, piece_id, piece_type_id, city_id, piece_hits):
		# Note that the server reuse unused id.
		# The newly created piece must not already be a player piece (a
		# delete_piece must have been processed before).
		#assert piece_id not in self.player_pieces
		#assert piece_id not in self.enemy_pieces
		#assert city_id in self.player_cities
		piece = Piece(piece_id, piece_type_id, self.player_id, piece_hits)
		piece.parent = self.get_player_city(city_id)
		self.player_pieces[piece_id] = piece
		city = self.get_player_city(city_id)
		city.content.append(piece)
		# Reset the production to force a new choice of piece type.
		self.player_cities[city_id].production = None

	def leave_city(self, piece_id, city_id):
		#assert piece_id in self.player_pieces
		#assert piece_id not in self.enemy_pieces
		#assert city_id in self.player_cities
		piece = self.get_player_piece(piece_id)
		piece.parent = None
		city = self.get_player_city(city_id)
		#assert piece in city.content
		city.content.remove(piece)

	def enter_city(self, piece_id, city_id):
		#assert piece_id in self.player_pieces
		#assert piece_id not in self.enemy_pieces
		#assert city_id in self.player_cities
		piece = self.get_player_piece(piece_id)
		city = self.get_player_city(city_id)
		# Before entering in a city, the piece must have leaved its previous location.
		#assert piece.parent == None
		piece.parent = city
		#assert piece not in city.content
		city.content.append(piece)

	def leave_piece(self, piece_id, transport_piece_id):
		#assert piece_id != transport_piece_id
		#assert piece_id in self.player_pieces
		#assert transport_piece_id in self.player_pieces
		#assert piece_id not in self.enemy_pieces
		#assert transport_piece_id not in self.enemy_pieces
		piece = self.get_player_piece(piece_id)
		transport = self.get_player_piece(transport_piece_id)
		#assert piece_id in transport.transport
		#assert piece.parent == transport
		#assert piece != transport
		transport.transport.remove(piece_id)
		piece.parent = None

	def enter_piece(self, piece_id, transport_piece_id):
		#assert piece_id != transport_piece_id
		#assert piece_id in self.player_pieces
		#assert transport_piece_id in self.player_pieces
		#assert piece_id not in self.enemy_pieces
		#assert transport_piece_id not in self.enemy_pieces
		piece = self.get_player_piece(piece_id)
		transport = self.get_player_piece(transport_piece_id)
		#assert piece_id not in transport.transport
		#assert piece.parent == None
		#assert piece != transport
		transport.transport.append(piece_id)
		piece.parent = transport

	def leave_terrain(self, piece_id, location):
		#assert piece_id in self.player_pieces
		#assert piece_id not in self.enemy_pieces
		piece = self.get_player_piece(piece_id)
		tile = self.get_tile(location)
		#assert tile.content == piece
		#assert piece.get_location() == location
		piece.parent = None
		tile.content = None

	def move(self, piece_id, location):
		# Do nothing. Associated event will change the situation
		# (set_visible).
		#assert piece_id in self.player_pieces
		pass

	def invade_city(self, city_id, location):
		tile = self.get_tile(location)
		tile.visible = True
		#assert tile.content != None
		#assert not isinstance(tile.content, Piece)
		# Must not belong to player, otherwise it is not an invasion!
		#assert not isinstance(tile.content, OwnedCity) or tile.content.owner != self.player_id
		if city_id in self.free_cities:
			del self.free_cities[city_id]
		else:
			del self.enemy_cities[city_id]
		city = OwnedCity(city_id, tile, self.player_id)
		tile.content = city
		self.player_cities[city_id] = city

	def lose_city(self, city_id):
		# The city is lost. It is now owned by the enemy.
		# While waiting to know which enemy now own this city, we make it free.
		# Normally, one of the next messages is set_visible_owned_city.
		#assert city_id in self.player_cities
		#assert city_id not in self.free_cities
		city = self.player_cities[city_id]
		parent = city.parent
		#assert isinstance(parent, Tile)
		del self.player_cities[city_id]
		city = City(city_id, parent)
		self.free_cities[city_id] = city
		parent.content = city

	def delete_piece(self, piece_id):
		piece = self.get_player_piece(piece_id)
		# Before being deleted, the piece must have leaved its previous location.
		#assert piece.parent == None
		del self.player_pieces[piece_id]

	def end(self):
		self.show()
