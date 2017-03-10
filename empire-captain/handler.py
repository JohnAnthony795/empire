class Handler:

	def __init__(self, situation):
		pass

	def set_height(self, height):
		pass

	def set_width(self, width):
		pass

	def set_player_id(self, player_id):
		pass

	def set_piece_types(self, piece_types):
		pass

	def set_visible_none(self, location, terrain):
		pass

	def set_visible_owned_city(self, location, terrain, city_id, owner):
		pass

	def set_visible_city(self, location, terrain, city_id):
		pass

	def set_visible_piece(self, location, terrain, owner, piece_id, piece_type_id, piece_hits):
		pass

	def set_explored(self, location, terrain):
		pass

	def create_piece(self, piece_id, piece_type_id, city_id, piece_hits):
		pass

	def leave_city(self, piece_id, city_id):
		pass

	def leave_piece(self, piece_id, transport_piece_id):
		pass

	def enter_city(self, piece_id, city_id):
		pass

	def enter_piece(self, piece_id, transport_piece_id):
		pass

	def leave_terrain(self, piece_id, location):
		pass

	def move(self, piece_id, location):
		pass

	def invade_city(self, city_id, location):
		pass

	def lose_city(self, city_id):
		pass

	def delete_piece(self, piece_id):
		pass

	def end(self):
		pass
