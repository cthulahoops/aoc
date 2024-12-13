#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINES 64
#define MAX_LINE_LENGTH 64

typedef struct {
    int x;
    int y;
    char value;
} GridPoint;

typedef struct {
    GridPoint points[MAX_LINES * MAX_LINE_LENGTH];  // Worst case: every character is a point
    size_t size;
} PointList;

typedef struct {
    char *lines[MAX_LINES];
    size_t size;
} StringList;

void init_string_list(StringList *list) {
    list->size = 0;
}

int add_string(StringList *list, const char *str) {
    if (list->size >= MAX_LINES) {
        return 0;
    }
    
    list->lines[list->size] = strdup(str);
    if (list->lines[list->size] == NULL) {
        fprintf(stderr, "Failed to allocate memory for string\n");
        exit(1);
    }
    list->size++;
    return 1;
}

void free_string_list(StringList *list) {
    for (size_t i = 0; i < list->size; i++) {
        free(list->lines[i]);
    }
    list->size = 0;
}


int count_chars(const StringList *grid, char value) {
    size_t count = 0;
    for (size_t i = 0; i < grid->size; i++) {
        for (size_t j = 0; j < strlen(grid->lines[i]); j++) {
            if (grid->lines[i][j] == value) {
                count++;
            }
        }
    }

    return count;
}

void init_point_list(PointList *list) {
    list->size = 0;
}

void add_point(PointList *list, int x, int y, char value) {
    if (list->size >= MAX_LINES * MAX_LINE_LENGTH) {
        fprintf(stderr, "Point list is full\n");
        exit(1);
    }

    list->points[list->size].x = x;
    list->points[list->size].y = y;
    list->points[list->size].value = value;
    list->size++;
}

void extract_points(StringList *grid, PointList *points) {
    init_point_list(points);

    printf("Grid size: %zu x %zu\n", grid->size, strlen(grid->lines[0]));
    for (size_t y = 0; y < grid->size; y++) {
        char *line = grid->lines[y];
        for (size_t x = 0; x < strlen(line); x++) {
            char c = line[x];
            if (c != '.' && c != ' ') {
                add_point(points, x, y, c);
            }
        }
    }
}

int mark_antipodes(const PointList * points, const StringList * grid, int part) {
    int start;
    int end;
    int step;

    if (part == 1) {
        start = -1;
        end = 3;
        step = 3;
    } else {
        start = -50;
        end = 50;
        step = 1;
    }

    for (size_t i = 0; i < points->size; i++) {
        for (size_t j = i + 1; j < points->size; j++) {
            if (points->points[i].value != points->points[j].value) {
                continue;
            }

            int dx = points->points[j].x - points->points[i].x;
            int dy = points->points[j].y - points->points[i].y;

            for (int s = start; s < end; s += step) {
                int x = points->points[i].x + s * dx;
                int y = points->points[i].y + s * dy;

                if (x >= 0 && x < strlen(grid->lines[0]) && y >= 0 && y < grid->size) {
                    grid->lines[y][x] = '#';
                }
            }
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "r");
    if (file == NULL) {
        perror("Error opening file");
        return 1;
    }

    printf("Reading file: %s\n", argv[1]);

    StringList grid;
    init_string_list(&grid);

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file) && grid.size < MAX_LINES) {
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') {
            line[len-1] = '\0';
        }
        if (!add_string(&grid, line)) {
            printf("Warning: Maximum number of lines (%d) reached\n", MAX_LINES);
            break;
        }
    }
    fclose(file);

    PointList points;
    extract_points(&grid, &points);

    printf("Found %zu points:\n", points.size);

    mark_antipodes(&points, &grid, 1);
    int part1 = count_chars(&grid, '#');

    mark_antipodes(&points, &grid, 2);
    int part2 = count_chars(&grid, '#');

    for (size_t i = 0; i < grid.size; i++) {
        printf("%s\n", grid.lines[i]);
    }

    printf("Part 1: %d\n", part1);
    printf("Part 2: %d\n", part2);

    free_string_list(&grid);
    return 0;
}
