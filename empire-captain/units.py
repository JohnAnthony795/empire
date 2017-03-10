import collections

import handler

# This class memorizes the positions of the player's units and enemy's units.
class Units(handler.Handler):

	OWNED_CITY=0
	CITY=1
	PIECE=2

	def __init__(self):
		self.date = 0
		self.city_cons = collections.namedtuple('City', ['date', 'visible', 'type'])
		self.owned_city_cons = collections.namedtuple('OwnedCity', ['date', 'visible', 'type', 'owner'])
		self.piece_cons = collections.namedtuple('Piece', ['date', 'visible', 'type', 'owner', 'piece_type_id'])

	def show(self):
		print "UNITS: (current date: %d)" % self.date
		for r in range(self.height):
			line = "  " * r
			for q in range(self.width):
				tile = self.view[q][r]
				if tile is None:
					info = ""
				elif tile.type == self.CITY:
					info = "c"
				elif tile.type == self.OWNED_CITY and tile.visible:
					info = "C%d" % tile.owner
				elif tile.type == self.OWNED_CITY:
					info = "?C%d" % tile.date
				elif tile.type == self.PIECE and tile.visible:
					info = "P%d" % tile.owner
				elif tile.type == self.PIECE:
					info = "?P%d" % tile.date
				else:
					assert False # Unknown tile content! Integrity error!
				line = line + (" " * (4 - len(info))) + info
			print line

	def next_turn(self):
		self.date = self.date + 1

	def get(self, location):
		q, r = location
		return self.view[q][r]

	def set_height(self, height):
		self.height = height
		if hasattr(self, "width"):
			self.view = [[None for r in range(self.height)] for q in range(self.width)] 

	def set_width(self, width):
		self.width = width
		if hasattr(self, "height"):
			self.view = [[None for r in range(self.height)] for q in range(self.width)] 

	def set_visible_none(self, location, terrain):
		q, r = location
		self.view[q][r] = None

	def set_visible_owned_city(self, location, terrain, city_id, owner):
		q, r = location
		self.view[q][r] = self.owned_city_cons(self.date, True, self.OWNED_CITY, owner)

	def set_visible_city(self, location, terrain, city_id):
		q, r = location
		self.view[q][r] = self.city_cons(self.date, True, self.CITY)

	def set_visible_piece(self, location, terrain, owner, piece_id, piece_type_id, piece_hits):
		q, r = location
		self.view[q][r] = self.piece_cons(self.date, True, self.PIECE, owner, piece_type_id)

	def set_explored(self, location, terrain):
		q, r = location
		cell = self.view[q][r]
		if cell is not None:
			self.view[q][r] = cell._replace(visible=False, date=self.date)
