# This file will (hopefully) read all the little WoW AH files and make a csv.
# One file to unite them all! (It is like 3.5 GB of text.)
# Needs to:
# Read directory and file name structure so I don't have to type it in manually.
# Make that a data structure of some sort, easy to read so the program can scan through it.
# Writes to a CSV.
# Data:  http://mmnet.iis.sinica.edu.tw/dl/wowah/


# PACKAGES
import csv
import re
import os           # This is for os.listdir
import os.path      # This is for the other dir stuff.
import string       # Maybe for directory name cycling etc.
import time			# For timing how long it takes.

# VARIABLES
max_char = 0		# Tracks char ID for error testing.
max_guild = 0		# Tracks guild ID for error testing.
mfc = 0				# Tracks how many little files were counted.
write_data_loc = 'c:/scripts/'       	     						# Adjust to your dir as needed.
write_data_file_elf = 'wowah_elf.csv'
write_data_file_orc = 'wowah_orc.csv'
write_data_file_tauren = 'wowah_tauren.csv'
write_data_file_troll = 'wowah_troll.csv'
write_data_file_undead= 'wowah_undead.csv'
write_data_file_unknown = 'wowah_unknown.csv'

#write_data_filename = write_data_loc + write_data_file
the_dir = 'C:/Scripts/WoWAH/'								# This is where the WoWAH folders are located, adjust as needed. Have them in their own subdir.

#REGEX								d		t		q			ID	G			Lev		R		C			Z
line_re = re.compile(r'^.*"[\d+],\s(.*),\s(\d+),(\d+),\s?(\d*),\s?(\d*),\s?(.*),\s(.*),\s(.*),.*".*$')
#                                  dummy   time    seq  char   guild

# REGEX NOTES
# groups: 1=timestamp, 3=avatarID, 4=guild.
# [1] = "0, 03/30/06 23:59:49, 1,10772, , 1, Orc, Warrior, Orgrimmar, , 0",
#       "0, 01/10/09 00:03:50, 1,55517, , 3, Orc, Warlock, Orgrimmar, WARLOCK, 0", -- [1]
#    	"0, 01/10/09 00:04:10, 5,4002,1, 75, Orc, Hunter, Zul'Gurub, HUNTER, 0", -- [26]
#       "0, 01/10/09 00:04:10, 5,78122,342, 80, Orc, Hunter, The Storm Peaks, HUNTER, 0", -- [32]
#   	"0, 01/10/09 00:08:04, 51,64635,161, 80, Blood Elf, Paladin, The Obsidian Sanctum, PALADIN, 0", -- [447]
# dummy, query time, query sequence number, avatar ID, guild, level, race, class, zone, dummy, dummy



# FUNCTIONS

def get_subdirs(the_folder):
    this_list = []
    this_list = os.listdir(the_folder)
    print 'From get_subdirs, a list is: ', this_list      # Printing for error control.
    for item in this_list:
        if item.startswith('.'):
            this_list.remove(item)
    return(this_list)
# End of get_subdirs
# '.DS_Store'


def get_file_list(the_folder):				# Yes these two are the same, just diff names.
    this_list = []
    this_list = os.listdir(the_folder)
    for item in this_list:
        if item.startswith('.'):
            this_list.remove(item)
    return(this_list)
# End of get_file_list


def parse_and_write(file, elf, orc, tauren, troll, undead, un):
	for line in file:                           # Oh the first "line" is a hard return???
#       print 'A line is: ', line
		data = line_re.match(line)
		# 0 - Dummy
		# 1 - TimeStampe
		# 2 - Query Sequency
		# 3 - Avatar ID
		# 4 -Guild
		# 5 - Level
		# 6 - race
		# 7 - class
		# 8 - Zone
		if data is not(None):
			#timestamp
			timestamp = data.group(1)
			#Fuck query sequence
			
			#Avatar ID
			char = data.group(3)
			
			#guild
			if data.group(4) is not(''):
				guild = data.group(4)
			else:
				guild = '-1'   # Note there are some missing values, i.e. errant -1.
				
			#level
			if data.group(5) is not (''):
				level = data.group(5)
			else:
				level = '1'
			# race & class
			if data.group(6) is not (''):
				race = data.group(6)
			else:
				race = ''
			#class
			if data.group(7) is not (''):
				zone = data.group(7)
			else:
				zone = ''


			#print timestamp    # This is so you can keep track of where it is. Max Jan 2009 IIRC.
			
			
			new_line = timestamp + ',' + char + ',' + guild + ',' + level + ',' + race + ',' + zone + '\n'
			if race.split(",")[0] == 'Orc':
				orc.write(new_line)
			elif race.split(",")[0]  == 'Tauren':
				tauren.write(new_line)
			elif race.split(",")[0]  == 'Troll':
				troll.write(new_line)
			elif race.split(",")[0]  == 'Undead':
				undead.write(new_line)
			elif race.split(",")[0]  == 'Blood Elf':
				elf.write(new_line)
			else:
				un.write(new_line)
                
		#else:
		#	print "Didn't match the regex."

# End of parse_and_write
# Note the two diff formats they use in the files, it changes partway through:
# [1] = "0, 03/30/06 23:59:49, 1,10772, , 1, Orc, Warrior, Orgrimmar, , 0",
#       "0, 01/10/09 00:03:50, 1,55517, , 3, Orc, Warlock, Orgrimmar, WARLOCK, 0", -- [1]
# dummy, query time, query sequence number, avatar ID, guild, level, race, class, zone, dummy, dummy


def read_tree(elf, orc, tauren, troll, undead, un):
	global the_dir
	months_folders = get_subdirs(the_dir)		# This is why the subdirs should be in their own location that you set in the vars section up top.
	for folder in months_folders:                                   # Run isdir(dir) first, try/except. Make sure no funny folders/dirs.
		folder = the_dir + folder                                   # Expands the folder name to the long version.
		day_folders = get_subdirs(folder)
		for day_folder in day_folders:
			day_folder = folder + '/' + day_folder
			file_list = get_file_list(day_folder)
			for file in file_list:
				try:
					file = day_folder + '/' + file
					print(file)
					with open(file, 'r') as f:
						this_file = f.readlines()                          # Should read the whole file as a string?
						parse_and_write(this_file, elf, orc, tauren, troll, undead, un)
				except IOError:
					print 'Error opening hoped for data-text file,', str(file), ', reason: ', IOError
# End of read_tree


def main():	
    #open write file here
	elf = open(write_data_loc + write_data_file_elf , 'a')    # 'a' is very important, it appends the new data to the big file. 
	orc = open(write_data_loc + write_data_file_orc, 'a') 
	tauren = open(write_data_loc + write_data_file_tauren, 'a') 
	troll = open(write_data_loc + write_data_file_troll, 'a') 
	undead = open(write_data_loc + write_data_file_undead, 'a') 
	un = open(write_data_loc + write_data_file_unknown, 'a') 
	
	fieldnames = ('timestamp,char, guild,level,race,class,zone\n')
	elf.write(fieldnames)
	orc.write(fieldnames)
	tauren.write(fieldnames)
	troll.write(fieldnames)
	undead.write(fieldnames)
	un.write(fieldnames)
	
	start_time = time.time()
	read_tree(elf, orc, tauren, troll, undead,un)
	#close write file here
	
	elf.close()
	orc.close()
	tauren.close()
	troll.close()
	undead.close()
	un.close()
	
	spent_time = time.time() - start_time
	mins_spent = int(spent_time / 60)
	secs_remainder = int(spent_time % 60)
	print 'Time of process: ', mins_spent, ':', secs_remainder     # 13m:42s on iMac. Also 14m:39s another time.
    
#    print 'Files scanned (or tried), ', mfc     # 138,084
#    print 'Max Chars: ', max_char               # They say 91,065 ">= 1" but it starts at 0, my count says: 91064 + 1 = 91,065.
#    print 'Max Guilds: ', max_guild             # They say "An integer within [1, 513]" but no since they start at 0. 512 + 1 = 513.
# End of main


# Main call

main()