import re
import sys

import situation

class Parser:
	def __init__(self, the_situation, handlers):
		self.handlers = handlers
		self.the_situation = the_situation
		self.re_transport_in_city = re.compile(r"transport in-city (\d+) (\d+)")

		# The order matters: it must be the same of both rexes and handlers (see parse method).
		self.rexes =	[ re.compile(r"set_visible (\d+) (\d+) (\w+) none")
				, re.compile(r"set_visible (\d+) (\d+) (\w+) owned_city (\d+) (\d+)")
				, re.compile(r"set_visible (\d+) (\d+) (\w+) city (\d+)")
				, re.compile(r"set_visible (\d+) (\d+) (\w+) piece (\d+) (\d+) (\d+) (\d+)")
				, re.compile(r"set_explored (\d+) (\d+) (\w+)")
				, re.compile(r"width (\d+)")
				, re.compile(r"height (\d+)")
				, re.compile(r"piece_types (.*)")
				, re.compile(r"player_id (\d+)")
				, re.compile(r"create_piece (\d+) (\d+) (\d+) (\d+)")
				, re.compile(r"delete_piece (\d+)")
				, re.compile(r"enter_city (\d+) (\d+)")
				, re.compile(r"enter_piece (\d+) (\d+)")
				, re.compile(r"leave_piece (\d+) (\d+)")
				, re.compile(r"leave_city (\d+) (\d+)")
				, re.compile(r"leave_terrain (\d+) (\d+) (\d+)")
				, re.compile(r"move (\d+) (\d+) (\d+)")
				, re.compile(r"invade_city (\d+) (\d+) (\d+)")
				, re.compile(r"lose_city (\d+)")
				, re.compile(r"winner (\d+)")
				, re.compile(r"random_seed (\d+)")
				, re.compile(r"ko-invasion (\d+) (\d+) (\d+)")
				, re.compile(r"ok-invasion (\d+) (\d+) (\d+)")
				, re.compile(r"created-units-limit (\d+)")
				, re.compile(r"city-units-limit (\d+)")
				]
		self.proxy_handlers =	[ self.parse_set_visible_none
				, self.parse_set_visible_owned_city
				, self.parse_set_visible_city
				, self.parse_set_visible_piece
				, self.parse_set_explored
				, self.parse_width
				, self.parse_height
				, self.parse_piece_types
				, self.parse_player_id
				, self.parse_create_piece
				, self.parse_delete_piece
				, self.parse_enter_city
				, self.parse_enter_piece
				, self.parse_leave_piece
				, self.parse_leave_city
				, self.parse_leave_terrain
				, self.parse_move
				, self.parse_invade_city
				, self.parse_lose_city
				, self.parse_winner
				, self.parse_random_seed
				, self.parse_ko_invasion
				, self.parse_ok_invasion
				, self.parse_created_units_limit
				, self.parse_city_units_limit
				]

	def parse_set_visible_none(self, groups):
		location = int(groups.group(1)), int(groups.group(2))
		terrain = self.the_situation.GROUND if groups.group(3) == "ground" else self.the_situation.WATER
		self.the_situation.set_visible_none(location, terrain)
		for handler in self.handlers:
			handler.set_visible_none(location, terrain)

	def parse_set_visible_owned_city(self, groups):
		location = int(groups.group(1)), int(groups.group(2))
		terrain = self.the_situation.GROUND if groups.group(3) == "ground" else self.the_situation.WATER
		city_id = int(groups.group(4))
		owner = int(groups.group(5))
		self.the_situation.set_visible_owned_city(location, terrain, city_id, owner)
		for handler in self.handlers:
			handler.set_visible_owned_city(location, terrain, city_id, owner)

	def parse_set_visible_city(self, groups):
		location = int(groups.group(1)), int(groups.group(2))
		terrain = self.the_situation.GROUND if groups.group(3) == "ground" else self.the_situation.WATER
		city_id = int(groups.group(4))
		self.the_situation.set_visible_city(location, terrain, city_id)
		for handler in self.handlers:
			handler.set_visible_city(location, terrain, city_id)

	def parse_set_visible_piece(self, groups):
		location = int(groups.group(1)), int(groups.group(2))
		terrain = self.the_situation.GROUND if groups.group(3) == "ground" else self.the_situation.WATER
		owner = int(groups.group(4))
		piece_id = int(groups.group(5))
		piece_type_id = int(groups.group(6))
		piece_hits = int(groups.group(7))
		self.the_situation.set_visible_piece(location, terrain, owner, piece_id, piece_type_id, piece_hits)
		for handler in self.handlers:
			handler.set_visible_piece(location, terrain, owner, piece_id, piece_type_id, piece_hits)

	def parse_set_explored(self, groups):
		location = int(groups.group(1)), int(groups.group(2))
		terrain = self.the_situation.GROUND if groups.group(3) == "ground" else self.the_situation.WATER
		self.the_situation.set_explored(location, terrain)
		for handler in self.handlers:
			handler.set_explored(location, terrain)

	def parse_width(self, groups):
		self.the_situation.set_width(int(groups.group(1)))
		for handler in self.handlers:
			handler.set_width(int(groups.group(1)))

	def parse_leave_city(self, groups):
		self.the_situation.leave_city(int(groups.group(1)), int(groups.group(2)))
		for handler in self.handlers:
			handler.leave_city(int(groups.group(1)), int(groups.group(2)))

	def parse_enter_city(self, groups):
		self.the_situation.enter_city(int(groups.group(1)), int(groups.group(2)))
		for handler in self.handlers:
			handler.enter_city(int(groups.group(1)), int(groups.group(2)))

	def parse_enter_piece(self, groups):
		self.the_situation.enter_piece(int(groups.group(1)), int(groups.group(2)))
		for handler in self.handlers:
			handler.enter_piece(int(groups.group(1)), int(groups.group(2)))

	def parse_leave_piece(self, groups):
		self.the_situation.leave_piece(int(groups.group(1)), int(groups.group(2)))
		for handler in self.handlers:
			handler.leave_piece(int(groups.group(1)), int(groups.group(2)))

	def parse_leave_terrain(self, groups):
		location = int(groups.group(2)), int(groups.group(3))
		self.the_situation.leave_terrain(int(groups.group(1)), location)
		for handler in self.handlers:
			handler.leave_terrain(int(groups.group(1)), location)

	def parse_height(self, groups):
		self.the_situation.set_height(int(groups.group(1)))
		for handler in self.handlers:
			handler.set_height(int(groups.group(1)))

	def parse_player_id(self, groups):
		self.the_situation.set_player_id(int(groups.group(1)))
		for handler in self.handlers:
			handler.set_player_id(int(groups.group(1)))

	def parse_piece_types(self, groups):
		piece_types = groups.group(1).split(";")
		piece_types = [x.split("#") for x in piece_types]
		result = {}
		terrain = {"water": self.the_situation.WATER, "ground": self.the_situation.GROUND}
		for piece_type in piece_types:
			info = {}
			info["piece_type_id"] = int(piece_type[0])
			info["name"] = piece_type[1]
			info["symbol"] = piece_type[2]
			info["terrains"] = [terrain[x] for x in piece_type[3].split(":")]
			info["build_time"] = int(piece_type[4])
			info["strength"] = int(piece_type[5])
			info["max_hits"] = int(piece_type[6])
			info["speed"] = int(piece_type[7])
			info["capacity"] = int(piece_type[8])
			info["autonomy"] = None if piece_type[9] == "" else int(piece_type[9])
			info["transportable"] = [] if piece_type[10] == "" else [int(x) for x in piece_type[10].split(":")]
			info["visibility"] = int(piece_type[11])
			info["can_invade"] = piece_type[12] in ["true", "True"]
			result[info["piece_type_id"]] = situation.PieceType(info)
		self.the_situation.set_piece_types(result)
		for handler in self.handlers:
			handler.set_piece_types(result)

	def parse_create_piece(self, groups):
		piece_id = int(groups.group(1))
		piece_type_id = int(groups.group(2))
		city_id = int(groups.group(3))
		piece_hits = int(groups.group(4))
		self.the_situation.create_piece(piece_id, piece_type_id, city_id, piece_hits)
		for handler in self.handlers:
			handler.create_piece(piece_id, piece_type_id, city_id, piece_hits)

	def parse_delete_piece(self, groups):
		piece_id = int(groups.group(1))
		self.the_situation.delete_piece(piece_id)
		for handler in self.handlers:
			handler.delete_piece(piece_id)

	def parse_move(self, groups):
		piece_id = int(groups.group(1))
		location = int(groups.group(2)), int(groups.group(3))
		self.the_situation.move(piece_id, location)
		for handler in self.handlers:
			handler.move(piece_id, location)

	def parse_invade_city(self, groups):
		city_id = int(groups.group(1))
		location = int(groups.group(2)), int(groups.group(3))
		self.the_situation.invade_city(city_id, location)
		for handler in self.handlers:
			handler.invade_city(city_id, location)

	def parse_lose_city(self, groups):
		city_id = int(groups.group(1))
		self.the_situation.lose_city(city_id)
		for handler in self.handlers:
			handler.lose_city(city_id)

	def parse_winner(self, groups):
		if int(groups.group(1)) == self.the_situation.player_id:
			print "Winner is you"
		else:
			print "Winner is the enemy"
		self.the_situation.end()
		for handler in self.handlers:
			handler.end()
		sys.exit(0)

	def parse_ok_invasion(self, groups):
		pass

	def parse_ko_invasion(self, groups):
		pass

	def parse_random_seed(self, groups):
		pass

	def parse_created_units_limit(self, groups):
		pass

	def parse_city_units_limit(self, groups):
		pass

	def parse(self, message):
		for i in range(len(self.rexes)):
			groups = self.rexes[i].match(message)
			if groups:
				self.proxy_handlers[i](groups)
				return
		self.the_situation.show()
		raise Exception("error: not handled: " + message)
