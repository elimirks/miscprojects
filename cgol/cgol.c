#include <stdio.h>

FILE *ifp;
char *mode = "r";

int map_width, map_height;
int cur_y, cur_x;

void clear() {

	#ifdef _WIN32

	system("cls");

	#else

	system("clear");

	#endif

}

int main(int argc, char* argv[]) {

	if (argc == 2) {

		ifp = fopen(argv[1], mode);

	} else {

		printf("%s\n", "Please load a map file as the first argument.\nUsage: cgol maps/hello.cgolmap\n");

		return(0);
	
	}

	if (ifp == NULL) {

		printf("%s%s%s\n", "Could not find map file '", argv[1], "'!");
		return(1);

	}

	/* FIXME add file verification */

	fscanf(ifp, "%d", &map_height);
	fscanf(ifp, "%d", &map_width);
	
	short map[map_height][map_width];

	char line[5];

	cur_y = 0;

	while (fscanf(ifp, "%s", line) != EOF) {

		printf("%s\n", line);

		cur_x = 0;

		while (cur_x < map_width) {

			map[cur_y][cur_x] = (line[cur_x] == 'X') ? 1 : 0;

			cur_x++;

		}

		cur_y++;

	}

	/* Main loop */
	while (1) {
		
		/* Draw the map */

		clear();

		/* Top border*/
		cur_x = 0;

		while (cur_x < map_width + 2) {

			printf("%s", "-");

			cur_x++;

		}
	
		printf("\n");

		cur_y = 0;

		while (cur_y < map_height) {

			cur_x = 0;
		
			/* Left border */
			printf("%s", "|");

			while (cur_x < map_width) {

				printf("%s", (map[cur_y][cur_x]) ? "X" : " ");

				cur_x++;

			}
		
			/* Right border */
			printf("%s", "|");

			printf("\n");

			cur_y++;
		
		}

		/* Bottom border*/
		cur_x = 0;

		while (cur_x < map_width + 2) {

			printf("%s", "-");

			cur_x++;

		}
	
		printf("\n");

		sleep(1);

		/* Apply the rules of the game */

		cur_y = 0;

		short tmpmap[map_height][map_width];

		while (cur_y < map_height) {

			cur_x = 0;
		
			while (cur_x < map_width) {

				int neighbors = 0;
			
				neighbors += (cur_y - 1 >= 0 && cur_x - 1 >= 0 && map[cur_y - 1][cur_x - 1]) ? 1 : 0;
				neighbors += (cur_y - 1 >= 0 && map[cur_y - 1][cur_x]) ? 1 : 0;
				neighbors += (cur_y - 1 >= 0 && cur_x + 1 < map_width && map[cur_y - 1][cur_x + 1]) ? 1 : 0;
				neighbors += (cur_x + 1 < map_width && map[cur_y][cur_x + 1]) ? 1 : 0;
				neighbors += (cur_y + 1 < map_height && cur_x + 1 < map_width && map[cur_y + 1][cur_x + 1]) ? 1 : 0;
				neighbors += (cur_y + 1 < map_height && map[cur_y + 1][cur_x]) ? 1 : 0;
				neighbors += (cur_y + 1 < map_height && cur_x - 1 >= 0 && map[cur_y + 1][cur_x - 1]) ? 1 : 0;
				neighbors += (cur_x - 1 >= 0 && map[cur_y][cur_x - 1]) ? 1 : 0;
			
				if (map[cur_y][cur_x] == 0 && neighbors == 3) {

					tmpmap[cur_y][cur_x] = 1;

				} else if (map[cur_y][cur_x] == 1 && neighbors >= 2 && neighbors <= 3) {

					tmpmap[cur_y][cur_x] = 1;

				} else {

					tmpmap[cur_y][cur_x] = 0;

				}

				cur_x++;

			}
		
			cur_y++;
		
		}

		/* Copy the tmpmap to the proper map */
		
		cur_y = 0;

		while (cur_y < map_height) {

			cur_x = 0;
		
			while (cur_x < map_width) {

				map[cur_y][cur_x] = tmpmap[cur_y][cur_x];

				cur_x++;

			}
		
			cur_y++;
		
		}

	}

	return(0);

}

