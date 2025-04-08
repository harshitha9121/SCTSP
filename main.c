#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <stdbool.h>
#include <time.h>

#define MAX_DIMENSION 500
#define MAX_NO_IMPROVEMENT_ITERATIONS 500
#define K_MEDOIDS_ITERATIONS 20
#define NO_RUNS 10
#define NO_RESTARTS 10

typedef struct {
    int id;
    int size;
    int vertices[MAX_DIMENSION];
    int best_order[MAX_DIMENSION];
    double total_distance;
    int total_prize;
    double pd_ratio;
    int medoid;
} Cluster;

typedef struct {
    int customer_id;
    int prize;
} Profit;

typedef struct {
    int dimension;
    int set_count;
    int tmax;
    int **edge_weight;
    Cluster clusters[MAX_DIMENSION];
    Profit profits[MAX_DIMENSION];
} Dataset;

typedef struct {
    int vertex;
    int prize;
} VertexProfit;

int compareVerticesByProfit(const void *a, const void *b) {
    VertexProfit *v1 = (VertexProfit *)a;
    VertexProfit *v2 = (VertexProfit *)b;
    return v2->prize - v1->prize;
}

int compareClustersByProfit(const void *a, const void *b) {
    Cluster *clusterA = (Cluster *)a;
    Cluster *clusterB = (Cluster *)b;
    return clusterB->total_prize - clusterA->total_prize;
}

int compareClustersByPdRatio(const void *a, const void *b) {
    Cluster *clusterA = (Cluster *)a;
    Cluster *clusterB = (Cluster *)b;
    double pdA = clusterA->pd_ratio == DBL_MAX ? -DBL_MAX : clusterA->pd_ratio;
    double pdB = clusterB->pd_ratio == DBL_MAX ? -DBL_MAX : clusterB->pd_ratio;
    return (pdA > pdB) - (pdA < pdB); // Changed to descending order
}

char* getNextNonEmptyLine(FILE *file, char *line, int size) {
    while (fgets(line, size, file)) {
        char *trimmed = line;
        while (*trimmed == ' ' || *trimmed == '\t' || *trimmed == '\n') trimmed++;
        if (*trimmed != '\0') return line;
    }
    return NULL;
}

void readDataset(Dataset *dataset, const char *filename) {
    FILE *file = fopen(filename, "r");
    if (file == NULL) {
        perror("Error opening dataset file");
        exit(EXIT_FAILURE);
    }
    printf("Opened file: %s\n", filename);

    char line[1024 * 1024];
    int in_edge_section = 0, in_set_section = 0, in_prize_section = 0;
    int row = 0, set_count = 0;

    while (fgets(line, sizeof(line), file)) {
        line[strcspn(line, "\n")] = 0;
        if (strncmp(line, "DIMENSION:", 10) == 0) {
            sscanf(line, "DIMENSION: %d", &dataset->dimension);
            printf("Dimension: %d\n", dataset->dimension);
            if (dataset->dimension <= 0 || dataset->dimension > MAX_DIMENSION) {
                fprintf(stderr, "Error: Invalid dimension %d\n", dataset->dimension);
                fclose(file);
                exit(EXIT_FAILURE);
            }
            dataset->edge_weight = (int **)malloc(dataset->dimension * sizeof(int *));
            for (int i = 0; i < dataset->dimension; i++) {
                dataset->edge_weight[i] = (int *)calloc(dataset->dimension, sizeof(int));
            }
        } else if (strncmp(line, "SET:", 4) == 0) {
            sscanf(line, "SET: %d", &dataset->set_count);
            printf("Set count: %d\n", dataset->set_count);
            if (dataset->set_count > MAX_DIMENSION) {
                fprintf(stderr, "Error: set_count %d exceeds MAX_DIMENSION\n", dataset->set_count);
                fclose(file);
                exit(EXIT_FAILURE);
            }
        } else if (strncmp(line, "TMAX:", 5) == 0) {
            sscanf(line, "TMAX: %d", &dataset->tmax);
            printf("Tmax: %d\n", dataset->tmax);
        } else if (strcmp(line, "EDGE_WEIGHT_SECTION:") == 0) {
            in_edge_section = 1;
        } else if (strcmp(line, "SET_SECTION: Number_of_Vertex_In_Set Vertex_List:") == 0) {
            in_edge_section = 0;
            in_set_section = 1;
        } else if (strcmp(line, "PRIZE_SECTION: Customer_id Customer_prize:") == 0) {
            in_set_section = 0;
            in_prize_section = 1;
        } else if (strcmp(line, "EOF") == 0) {
            break;
        } else if (in_edge_section && row < dataset->dimension) {
            char *token = strtok(line, " \t\n");
            int col = 0;
            while (token && col < dataset->dimension) {
                dataset->edge_weight[row][col] = atoi(token);
                token = strtok(NULL, " \t\n");
                col++;
            }
            row++;
        } else if (in_set_section && set_count < dataset->set_count) {
            char *token = strtok(line, " \t\n");
            dataset->clusters[set_count].size = atoi(token);
            dataset->clusters[set_count].id = set_count + 1;
            token = strtok(NULL, " \t\n");
            for (int j = 0; j < dataset->clusters[set_count].size && token; j++) {
                dataset->clusters[set_count].vertices[j] = atoi(token);
                token = strtok(NULL, " \t\n");
            }
            set_count++;
        } else if (in_prize_section) {
            int id, prize;
            if (sscanf(line, "%d %d", &id, &prize) == 2) {
                dataset->profits[id - 1].customer_id = id;
                dataset->profits[id - 1].prize = prize;
            }
        }
    }
    if (row != dataset->dimension) {
        fprintf(stderr, "Error: Incomplete edge weight section\n");
        fclose(file);
        exit(EXIT_FAILURE);
    }
    if (dataset->dimension == 0 || dataset->set_count == 0 || dataset->tmax == 0) {
        fprintf(stderr, "Invalid dataset: dim=%d, sets=%d, tmax=%d\n", dataset->dimension, dataset->set_count, dataset->tmax);
        fclose(file);
        exit(EXIT_FAILURE);
    }
    fclose(file);
}

int calculateClusterPrize(Dataset *dataset, Cluster *cluster) {
    int total_prize = 0;
    for (int i = 0; i < cluster->size; ++i) {
        int vertex = cluster->vertices[i] - 1;
        total_prize += dataset->profits[vertex].prize;
    }
    return total_prize;
}

int findMedoid(Dataset *dataset, Cluster *cluster) {
    double min_total_dist = DBL_MAX;
    int best_medoid_idx = 0;
    for (int i = 0; i < cluster->size; ++i) {
        int current_vertex = cluster->vertices[i] - 1;
        double total_dist = 0;
        for (int j = 0; j < cluster->size; ++j) {
            if (i != j) {
                int other_vertex = cluster->vertices[j] - 1;
                total_dist += dataset->edge_weight[current_vertex][other_vertex];
            }
        }
        if (total_dist < min_total_dist) {
            min_total_dist = total_dist;
            best_medoid_idx = i;
        }
    }
    return cluster->vertices[best_medoid_idx];
}

double buildGreedyPath(Dataset *dataset, Cluster *cluster, int medoid, int *order) {
    bool *visited = malloc(cluster->size * sizeof(bool));
    if (!visited) {
        perror("Memory allocation failed for visited");
        exit(EXIT_FAILURE);
    }
    memset(visited, false, cluster->size * sizeof(bool));
    int order_size = 0;
    double total_distance = 0;

    order[0] = medoid;
    int medoid_idx = -1;
    for (int i = 0; i < cluster->size; ++i) {
        if (cluster->vertices[i] == medoid) {
            medoid_idx = i;
            break;
        }
    }
    visited[medoid_idx] = true;
    order_size = 1;

    for (int i = 1; i < cluster->size; ++i) {
        int last = order[order_size - 1] - 1;
        double min_dist = DBL_MAX;
        int next_idx = -1;
        for (int j = 0; j < cluster->size; ++j) {
            if (!visited[j]) {
                int v = cluster->vertices[j] - 1;
                double dist = dataset->edge_weight[last][v];
                if (dist < min_dist) {
                    min_dist = dist;
                    next_idx = j;
                }
            }
        }
        order[order_size++] = cluster->vertices[next_idx];
        visited[next_idx] = true;
        total_distance += min_dist;
    }
    free(visited);
    return total_distance;
}

void kMedoids(Dataset *dataset, Cluster *cluster) {
    if (cluster->size == 0) {
        cluster->total_distance = 0;
        cluster->total_prize = 0;
        cluster->pd_ratio = 0;
        return;
    }
    if (cluster->size <= 2) {
        memcpy(cluster->best_order, cluster->vertices, cluster->size * sizeof(int));
        cluster->total_distance = (cluster->size == 2) ? dataset->edge_weight[cluster->vertices[0] - 1][cluster->vertices[1] - 1] : 0;
    } else {
        double best_distance = DBL_MAX;
        int best_medoid = -1;
        int best_order[MAX_DIMENSION];

        for (int iter = 0; iter < K_MEDOIDS_ITERATIONS; ++iter) {
            int medoid = findMedoid(dataset, cluster);
            int temp_order[MAX_DIMENSION];
            double distance = buildGreedyPath(dataset, cluster, medoid, temp_order);
            if (distance < best_distance) {
                best_distance = distance;
                best_medoid = medoid;
                memcpy(best_order, temp_order, cluster->size * sizeof(int));
            }
        }
        cluster->medoid = best_medoid;
        memcpy(cluster->best_order, best_order, cluster->size * sizeof(int));
        cluster->total_distance = best_distance;
    }
    cluster->total_prize = calculateClusterPrize(dataset, cluster);
    cluster->pd_ratio = cluster->total_distance ? (double)cluster->total_prize / cluster->total_distance : DBL_MAX;
}

double calculateClusterDistance(Dataset *dataset, Cluster *cluster) {
    double total_distance = 0;
    for (int i = 0; i < cluster->size - 1; ++i) {
        int from = cluster->best_order[i] - 1;
        int to = cluster->best_order[i + 1] - 1;
        total_distance += dataset->edge_weight[from][to];
    }
    return total_distance;
}

double calculateClusterDistanceWithOrder(Dataset *dataset, Cluster *cluster, int *order) {
    double total_distance = 0;
    for (int i = 0; i < cluster->size - 1; ++i) {
        int from = order[i] - 1;
        int to = order[i + 1] - 1;
        total_distance += dataset->edge_weight[from][to];
    }
    return total_distance;
}

double calculateTotalDistance(Dataset *dataset, Cluster *clusters, int *visited_clusters_order, int order_size) {
    if (order_size < 1) return 0;
    double total_distance = 0;
    int last_vertex = -1;

    for (int i = 0; i < order_size; ++i) {
        int cluster_idx = visited_clusters_order[i] - 1;
        Cluster *cluster = &clusters[cluster_idx];
        if (cluster->size > 1) {
            for (int j = 0; j < cluster->size - 1; ++j) {
                int from = cluster->best_order[j] - 1;
                int to = cluster->best_order[j + 1] - 1;
                total_distance += dataset->edge_weight[from][to];
            }
        }
        if (last_vertex != -1) {
            int first_vertex = cluster->best_order[0] - 1;
            total_distance += dataset->edge_weight[last_vertex][first_vertex];
        }
        last_vertex = cluster->best_order[cluster->size - 1] - 1;
    }
    return total_distance;
}

int calculateTotalPrize(Dataset *dataset, int *visited_clusters_order, int order_size) {
    int *visited = malloc(MAX_DIMENSION * sizeof(int));
    if (!visited) {
        perror("Memory allocation failed for visited");
        exit(EXIT_FAILURE);
    }
    memset(visited, 0, MAX_DIMENSION * sizeof(int));
    int total_prize = 0;

    for (int i = 0; i < order_size; ++i) {
        int cluster_idx = visited_clusters_order[i] - 1;
        for (int j = 0; j < dataset->clusters[cluster_idx].size; ++j) {
            int vertex = dataset->clusters[cluster_idx].vertices[j] - 1;
            if (!visited[vertex]) {
                total_prize += dataset->profits[vertex].prize;
                visited[vertex] = 1;
            }
        }
    }
    free(visited);
    return total_prize;
}

void enforceCluster1StartEnd(int *order, int *size) {
    int *new_order = malloc(MAX_DIMENSION * sizeof(int));
    int new_size = 0;
    new_order[new_size++] = 1;
    for (int i = 0; i < *size; ++i) {
        if (order[i] != 1) {
            new_order[new_size++] = order[i];
        }
    }
    new_order[new_size++] = 1;
    memcpy(order, new_order, new_size * sizeof(int));
    *size = new_size;
    free(new_order);
}

void printSolution(FILE *file, Dataset *dataset, int *order, int size, double dist, int prize) {
    fprintf(file, "visiting_clusters_order: ");
    for (int i = 0; i < size; ++i) {
        fprintf(file, "%d", order[i]);
        if (i < size - 1) fprintf(file, " -> ");
    }
    fprintf(file, "\nunvisited clusters: ");
    bool first = true;
    for (int i = 0; i < dataset->set_count; ++i) {
        bool visited = false;
        for (int j = 0; j < size; ++j) {
            if (order[j] == dataset->clusters[i].id) {
                visited = true;
                break;
            }
        }
        if (!visited) {
            if (!first) fprintf(file, ", ");
            fprintf(file, "%d", dataset->clusters[i].id);
            first = false;
        }
    }
    if (first) fprintf(file, "None");
    fprintf(file, "\ndistance: %.2f\n", dist);
    fprintf(file, "profit: %d\n\n", prize);
}

void printClusterDetails(FILE *file, Dataset *dataset, int *order, int size) {
    for (int i = 0; i < size; ++i) {
        int cluster_idx = order[i] - 1;
        Cluster *cluster = &dataset->clusters[cluster_idx];
        fprintf(file, "cluster %d: ", cluster->id);
        for (int j = 0; j < cluster->size; ++j) {
            fprintf(file, "%d", cluster->best_order[j]);
            if (j < cluster->size - 1) fprintf(file, " -> ");
        }
        fprintf(file, "\n");
    }
    fprintf(file, "\n");
}

void makeS0Solution(Dataset *dataset, int *infeasible_order, int infeasible_size, int **s0_order, int *s0_size, double *s0_dist, int *s0_prize) {
    int temp_size = infeasible_size;
    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    memcpy(temp_order, infeasible_order, infeasible_size * sizeof(int));
    double current_dist = calculateTotalDistance(dataset, dataset->clusters, temp_order, temp_size);
    int current_prize = calculateTotalPrize(dataset, temp_order, temp_size);

    // Create a sorted array of clusters by p/d ratio (ascending order for removal)
    Cluster *sorted_clusters = malloc(dataset->set_count * sizeof(Cluster));
    memcpy(sorted_clusters, dataset->clusters, dataset->set_count * sizeof(Cluster));
    qsort(sorted_clusters, dataset->set_count, sizeof(Cluster), compareClustersByPdRatio);  // Low to high p/d

    bool visited[MAX_DIMENSION] = {false};
    for (int i = 0; i < temp_size; i++) {
        visited[temp_order[i] - 1] = true;
    }

    // Keep removing clusters with lowest p/d ratio until distance is under tmax
    while (current_dist > dataset->tmax && temp_size > 2) {
        double best_dist = DBL_MAX;
        int best_prize = 0;
        int remove_idx = -1;
        int remove_cluster_id = -1;

        // Try removing each cluster that's not cluster 1 (start/end)
        for (int i = 1; i < temp_size - 1; i++) {
            int cluster_id = temp_order[i];
            int *test_order = malloc(MAX_DIMENSION * sizeof(int));
            memcpy(test_order, temp_order, temp_size * sizeof(int));
            memmove(&test_order[i], &test_order[i + 1], (temp_size - i - 1) * sizeof(int));
            int test_size = temp_size - 1;

            double test_dist = calculateTotalDistance(dataset, dataset->clusters, test_order, test_size);
            int test_prize = calculateTotalPrize(dataset, test_order, test_size);

            // Find the removal that gives lowest distance while maintaining highest prize possible
            if (test_dist <= dataset->tmax && (test_dist < best_dist || 
                (test_dist == best_dist && test_prize > best_prize))) {
                best_dist = test_dist;
                best_prize = test_prize;
                remove_idx = i;
                remove_cluster_id = cluster_id;
            }
            free(test_order);
        }

        if (remove_idx != -1) {
            memmove(&temp_order[remove_idx], &temp_order[remove_idx + 1], (temp_size - remove_idx - 1) * sizeof(int));
            temp_size--;
            current_dist = best_dist;
            current_prize = best_prize;
            visited[remove_cluster_id - 1] = false;
        } else {
            // If no single removal works, try removing lowest p/d ratio cluster
            for (int i = 0; i < dataset->set_count; i++) {
                int cluster_id = sorted_clusters[i].id;
                if (cluster_id == 1 || !visited[cluster_id - 1]) continue;

                for (int j = 1; j < temp_size - 1; j++) {
                    if (temp_order[j] == cluster_id) {
                        int *test_order = malloc(MAX_DIMENSION * sizeof(int));
                        memcpy(test_order, temp_order, temp_size * sizeof(int));
                        memmove(&test_order[j], &test_order[j + 1], (temp_size - j - 1) * sizeof(int));
                        int test_size = temp_size - 1;

                        double test_dist = calculateTotalDistance(dataset, dataset->clusters, test_order, test_size);
                        int test_prize = calculateTotalPrize(dataset, test_order, test_size);

                        if (test_dist < best_dist || 
                            (test_dist == best_dist && test_prize > best_prize)) {
                            best_dist = test_dist;
                            best_prize = test_prize;
                            remove_idx = j;
                            remove_cluster_id = cluster_id;
                        }
                        free(test_order);
                    }
                }
                if (remove_idx != -1) break;
            }

            if (remove_idx != -1) {
                memmove(&temp_order[remove_idx], &temp_order[remove_idx + 1], (temp_size - remove_idx - 1) * sizeof(int));
                temp_size--;
                current_dist = best_dist;
                current_prize = best_prize;
                visited[remove_cluster_id - 1] = false;
            } else {
                break;  // No more improvements possible
            }
        }
    }

    *s0_order = realloc(*s0_order, temp_size * sizeof(int));
    memcpy(*s0_order, temp_order, temp_size * sizeof(int));
    *s0_size = temp_size;
    *s0_dist = current_dist;
    *s0_prize = current_prize;
    
    free(temp_order);
    free(sorted_clusters);
}

void tryInsertion(Dataset *dataset, int **order, int *size, double *dist, int *prize, FILE *file) {
    bool visited[MAX_DIMENSION] = {false};
    for (int i = 0; i < *size; ++i) {
        visited[(*order)[i] - 1] = true;
    }
    Cluster *unvisited_clusters = malloc(dataset->set_count * sizeof(Cluster));
    int unvisited_count = 0;
    for (int i = 0; i < dataset->set_count; ++i) {
        if (!visited[i]) {
            unvisited_clusters[unvisited_count] = dataset->clusters[i];
            unvisited_count++;
        }
    }
    qsort(unvisited_clusters, unvisited_count, sizeof(Cluster), compareClustersByProfit);

    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    int temp_size = *size;
    memcpy(temp_order, *order, *size * sizeof(int));
    bool inserted = false;

    for (int c = 0; c < unvisited_count; ++c) {
        int cluster_id = unvisited_clusters[c].id;
        int best_pos = -1;
        double best_dist = DBL_MAX;
        int best_prize = *prize;

        for (int pos = 1; pos < temp_size; ++pos) {
            int *new_order = malloc(MAX_DIMENSION * sizeof(int));
            int new_size = temp_size + 1;
            memcpy(new_order, temp_order, temp_size * sizeof(int));
            memmove(&new_order[pos + 1], &new_order[pos], (temp_size - pos) * sizeof(int));
            new_order[pos] = cluster_id;

            double new_dist = calculateTotalDistance(dataset, dataset->clusters, new_order, new_size);
            int new_prize = calculateTotalPrize(dataset, new_order, new_size);

            if (new_dist <= dataset->tmax && new_dist < best_dist) {
                best_dist = new_dist;
                best_prize = new_prize;
                best_pos = pos;
            }
            free(new_order);
        }

        if (best_pos != -1 && best_prize > *prize) {
            temp_size++;
            temp_order = realloc(temp_order, temp_size * sizeof(int));
            memmove(&temp_order[best_pos + 1], &temp_order[best_pos], (temp_size - best_pos - 1) * sizeof(int));
            temp_order[best_pos] = cluster_id;
            *dist = best_dist;
            *prize = best_prize;
            inserted = true;
            fprintf(file, "Insertion: Inserted cluster %d after heuristic\n", cluster_id);
            break;
        }
    }

    if (inserted) {
        *order = realloc(*order, temp_size * sizeof(int));
        memcpy(*order, temp_order, temp_size * sizeof(int));
        *size = temp_size;
    }

    free(temp_order);
    free(unvisited_clusters);
}

void evaluateAndApplyBestInsertion(Dataset *dataset, int *s0_order, int s0_size, int **s1_order, int *s1_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    bool visited[MAX_DIMENSION] = {false};
    for (int i = 0; i < s0_size; ++i) {
        visited[s0_order[i] - 1] = true;
    }
    Cluster *unvisited_clusters = malloc(dataset->set_count * sizeof(Cluster));
    int unvisited_count = 0;
    for (int i = 0; i < dataset->set_count; ++i) {
        if (!visited[i]) {
            unvisited_clusters[unvisited_count] = dataset->clusters[i];
            unvisited_count++;
        }
    }
    qsort(unvisited_clusters, unvisited_count, sizeof(Cluster), compareClustersByProfit);

    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    int temp_size = s0_size;
    memcpy(temp_order, s0_order, s0_size * sizeof(int));
    bool inserted = false;

    for (int c = 0; c < unvisited_count; ++c) {
        int cluster_id = unvisited_clusters[c].id;
        int best_pos = -1;
        double best_dist = DBL_MAX;
        int best_prize = s0_prize;

        for (int pos = 1; pos < temp_size; ++pos) {
            int *new_order = malloc(MAX_DIMENSION * sizeof(int));
            int new_size = temp_size + 1;
            memcpy(new_order, temp_order, temp_size * sizeof(int));
            memmove(&new_order[pos + 1], &new_order[pos], (temp_size - pos) * sizeof(int));
            new_order[pos] = cluster_id;

            double new_dist = calculateTotalDistance(dataset, dataset->clusters, new_order, new_size);
            int new_prize = calculateTotalPrize(dataset, new_order, new_size);

            if (new_dist <= dataset->tmax && new_dist < best_dist) {
                best_dist = new_dist;
                best_prize = new_prize;
                best_pos = pos;
            }
            free(new_order);
        }

        if (best_pos != -1 && best_prize > s0_prize) {
            temp_size++;
            temp_order = realloc(temp_order, temp_size * sizeof(int));
            memmove(&temp_order[best_pos + 1], &temp_order[best_pos], (temp_size - best_pos - 1) * sizeof(int));
            temp_order[best_pos] = cluster_id;
            s0_dist = best_dist;
            s0_prize = best_prize;
            inserted = true;
            fprintf(file, "Insertion: Inserting cluster %d\n", cluster_id);
            break;
        }
    }

    *s1_order = realloc(*s1_order, temp_size * sizeof(int));
    memcpy(*s1_order, temp_order, temp_size * sizeof(int));
    *s1_size = temp_size;

    if (inserted) {
        fprintf(file, "S1 Solution (Insertion based on high profit):\n");
        printSolution(file, dataset, *s1_order, *s1_size, s0_dist, s0_prize);
        printClusterDetails(file, dataset, *s1_order, *s1_size);
    } else {
        fprintf(file, "S1 Solution: No insertion found under tmax\n\n");
    }

    free(temp_order);
    free(unvisited_clusters);
}

void evaluateAndApplyBestSwap(Dataset *dataset, int *s0_order, int s0_size, int **s2_order, int *s2_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    double best_dist = s0_dist;
    int best_cluster_idx = -1;
    int best_i = -1, best_j = -1;

    Cluster temp_clusters[MAX_DIMENSION];
    memcpy(temp_clusters, dataset->clusters, dataset->set_count * sizeof(Cluster));

    for (int k = 0; k < s0_size; ++k) {
        int cluster_idx = s0_order[k] - 1;
        Cluster *cluster = &temp_clusters[cluster_idx];
        if (cluster->size <= 1) continue;

        for (int i = 0; i < cluster->size - 1; ++i) {
            for (int j = i + 1; j < cluster->size; ++j) {
                int *temp_order = malloc(cluster->size * sizeof(int));
                memcpy(temp_order, cluster->best_order, cluster->size * sizeof(int));
                int temp = temp_order[i];
                temp_order[i] = temp_order[j];
                temp_order[j] = temp;

                double new_cluster_dist = calculateClusterDistanceWithOrder(dataset, cluster, temp_order);
                memcpy(cluster->best_order, temp_order, cluster->size * sizeof(int));
                cluster->total_distance = new_cluster_dist;

                double new_total_dist = calculateTotalDistance(dataset, temp_clusters, s0_order, s0_size);
                if (new_total_dist < best_dist && new_total_dist <= dataset->tmax) {
                    best_dist = new_total_dist;
                    best_cluster_idx = cluster_idx;
                    best_i = i;
                    best_j = j;
                }
                free(temp_order);
            }
        }
    }

    if (best_dist < s0_dist && best_cluster_idx != -1) {
        Cluster *best_cluster = &dataset->clusters[best_cluster_idx];
        int temp = best_cluster->best_order[best_i];
        best_cluster->best_order[best_i] = best_cluster->best_order[best_j];
        best_cluster->best_order[best_j] = temp;
        best_cluster->total_distance = calculateClusterDistance(dataset, best_cluster);

        *s2_order = realloc(*s2_order, s0_size * sizeof(int));
        memcpy(*s2_order, s0_order, s0_size * sizeof(int));
        *s2_size = s0_size;

        fprintf(file, "S2 Solution (Swap within clusters):\n");
        printSolution(file, dataset, *s2_order, *s2_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s2_order, *s2_size);

        tryInsertion(dataset, s2_order, s2_size, &best_dist, &s0_prize, file);
        fprintf(file, "S2 Solution after possible insertion:\n");
        printSolution(file, dataset, *s2_order, *s2_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s2_order, *s2_size);
    } else {
        *s2_order = realloc(*s2_order, s0_size * sizeof(int));
        memcpy(*s2_order, s0_order, s0_size * sizeof(int));
        *s2_size = s0_size;
        fprintf(file, "S2 Solution: No swap found under tmax\n\n");
    }
}

void evaluateAndApplyBest2Opt(Dataset *dataset, int *s0_order, int s0_size, int **s3_order, int *s3_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    double best_dist = s0_dist;
    int best_cluster_idx = -1;
    int best_i = -1, best_j = -1;

    Cluster temp_clusters[MAX_DIMENSION];
    memcpy(temp_clusters, dataset->clusters, dataset->set_count * sizeof(Cluster));

    for (int k = 0; k < s0_size; ++k) {
        int cluster_idx = s0_order[k] - 1;
        Cluster *cluster = &temp_clusters[cluster_idx];
        if (cluster->size <= 2) continue;

        for (int i = 0; i < cluster->size - 1; ++i) {
            for (int j = i + 2; j < cluster->size; ++j) {
                int *temp_order = malloc(cluster->size * sizeof(int));
                memcpy(temp_order, cluster->best_order, cluster->size * sizeof(int));
                for (int left = i, right = j; left < right; ++left, --right) {
                    int temp = temp_order[left];
                    temp_order[left] = temp_order[right];
                    temp_order[right] = temp;
                }

                double new_cluster_dist = calculateClusterDistanceWithOrder(dataset, cluster, temp_order);
                memcpy(cluster->best_order, temp_order, cluster->size * sizeof(int));
                cluster->total_distance = new_cluster_dist;

                double new_total_dist = calculateTotalDistance(dataset, temp_clusters, s0_order, s0_size);
                if (new_total_dist < best_dist && new_total_dist <= dataset->tmax) {
                    best_dist = new_total_dist;
                    best_cluster_idx = cluster_idx;
                    best_i = i;
                    best_j = j;
                }
                free(temp_order);
            }
        }
    }

    if (best_dist < s0_dist && best_cluster_idx != -1) {
        Cluster *best_cluster = &dataset->clusters[best_cluster_idx];
        int *temp_order = malloc(best_cluster->size * sizeof(int));
        memcpy(temp_order, best_cluster->best_order, best_cluster->size * sizeof(int));
        for (int left = best_i, right = best_j; left < right; ++left, --right) {
            int temp = temp_order[left];
            temp_order[left] = temp_order[right];
            temp_order[right] = temp;
        }
        memcpy(best_cluster->best_order, temp_order, best_cluster->size * sizeof(int));
        best_cluster->total_distance = calculateClusterDistance(dataset, best_cluster);
        free(temp_order);

        *s3_order = realloc(*s3_order, s0_size * sizeof(int));
        memcpy(*s3_order, s0_order, s0_size * sizeof(int));
        *s3_size = s0_size;

        fprintf(file, "S3 Solution (2-opt within clusters):\n");
        printSolution(file, dataset, *s3_order, *s3_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s3_order, *s3_size);

        tryInsertion(dataset, s3_order, s3_size, &best_dist, &s0_prize, file);
        fprintf(file, "S3 Solution after possible insertion:\n");
        printSolution(file, dataset, *s3_order, *s3_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s3_order, *s3_size);
    } else {
        *s3_order = realloc(*s3_order, s0_size * sizeof(int));
        memcpy(*s3_order, s0_order, s0_size * sizeof(int));
        *s3_size = s0_size;
        fprintf(file, "S3 Solution: No 2-opt found under tmax\n\n");
    }
}

void evaluateAndApplyBestRelocation(Dataset *dataset, int *s0_order, int s0_size, int **s4_order, int *s4_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    bool improved = false;

    Cluster temp_clusters[MAX_DIMENSION];
    memcpy(temp_clusters, dataset->clusters, dataset->set_count * sizeof(Cluster));

    for (int k = 0; k < s0_size; ++k) {
        int cluster_idx = s0_order[k] - 1;
        Cluster *cluster = &temp_clusters[cluster_idx];
        if (cluster->size <= 1) continue;

        VertexProfit *removed_cities = malloc(cluster->size * sizeof(VertexProfit));
        int removed_count = 0;
        int *temp_order = malloc(cluster->size * sizeof(int));
        int temp_size = 0;
        memcpy(temp_order, cluster->best_order, cluster->size * sizeof(int));

        for (int i = 0; i < cluster->size; ++i) {
            double random = (float)rand() / RAND_MAX;
            if (random > 0.5) {
                removed_cities[removed_count].vertex = cluster->best_order[i];
                removed_cities[removed_count].prize = dataset->profits[cluster->best_order[i] - 1].prize;
                removed_count++;
            } else {
                temp_order[temp_size++] = cluster->best_order[i];
            }
        }

        for (int r = 0; r < removed_count; ++r) {
            int city = removed_cities[r].vertex;
            int best_pos = -1;
            double min_dist = DBL_MAX;
            int *new_order = malloc((temp_size + 1) * sizeof(int));

            for (int pos = 0; pos <= temp_size; ++pos) {
                memcpy(new_order, temp_order, temp_size * sizeof(int));
                memmove(&new_order[pos + 1], &new_order[pos], (temp_size - pos) * sizeof(int));
                new_order[pos] = city;

                Cluster temp_cluster = *cluster;
                temp_cluster.size = temp_size + 1;
                memcpy(temp_cluster.best_order, new_order, (temp_size + 1) * sizeof(int));
                double new_cluster_dist = calculateClusterDistanceWithOrder(dataset, &temp_cluster, new_order);

                if (new_cluster_dist < min_dist) {
                    min_dist = new_cluster_dist;
                    best_pos = pos;
                }
            }

            memmove(&temp_order[best_pos + 1], &temp_order[best_pos], (temp_size - best_pos) * sizeof(int));
            temp_order[best_pos] = city;
            temp_size++;
            free(new_order);
        }

        double new_cluster_dist = calculateClusterDistanceWithOrder(dataset, cluster, temp_order);
        memcpy(cluster->best_order, temp_order, cluster->size * sizeof(int));
        cluster->total_distance = new_cluster_dist;

        double new_total_dist = calculateTotalDistance(dataset, temp_clusters, s0_order, s0_size);
        if (new_total_dist < s0_dist && new_total_dist <= dataset->tmax) {
            memcpy(dataset->clusters[cluster_idx].best_order, temp_order, cluster->size * sizeof(int));
            dataset->clusters[cluster_idx].total_distance = new_cluster_dist;
            s0_dist = new_total_dist;
            improved = true;
        }

        free(temp_order);
        free(removed_cities);
    }

    *s4_order = realloc(*s4_order, s0_size * sizeof(int));
    memcpy(*s4_order, s0_order, s0_size * sizeof(int));
    *s4_size = s0_size;

    if (improved) {
        fprintf(file, "S4 Solution (Relocation within clusters):\n");
        printSolution(file, dataset, *s4_order, *s4_size, s0_dist, s0_prize);
        printClusterDetails(file, dataset, *s4_order, *s4_size);

        tryInsertion(dataset, s4_order, s4_size, &s0_dist, &s0_prize, file);
        fprintf(file, "S4 Solution after possible insertion:\n");
        printSolution(file, dataset, *s4_order, *s4_size, s0_dist, s0_prize);
        printClusterDetails(file, dataset, *s4_order, *s4_size);
    } else {
        fprintf(file, "S4 Solution: No relocation within clusters found\n\n");
    }
}

void evaluateAndApplyBestClusterSwap(Dataset *dataset, int *s0_order, int s0_size, int **s5_order, int *s5_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);

    // Clean initial order: ensure cluster 1 is only at start and end
    int *cleaned_s0_order = malloc(MAX_DIMENSION * sizeof(int));
    int cleaned_size = 0;
    cleaned_s0_order[0] = 1;
    cleaned_size++;
    for (int i = 1; i < s0_size - 1; ++i) {
        if (s0_order[i] != 1) {
            cleaned_s0_order[cleaned_size++] = s0_order[i];
        }
    }
    cleaned_s0_order[cleaned_size++] = 1;
    s0_dist = calculateTotalDistance(dataset, dataset->clusters, cleaned_s0_order, cleaned_size);
    s0_prize = calculateTotalPrize(dataset, cleaned_s0_order, cleaned_size);

    if (cleaned_size != s0_size) {
        fprintf(file, "S5 Warning: Initial order had extra cluster 1s, corrected\n");
        fprintf(file, "Cleaned initial order: ");
        for (int i = 0; i < cleaned_size; ++i) {
            fprintf(file, "%d", cleaned_s0_order[i]);
            if (i < cleaned_size - 1) fprintf(file, " -> ");
        }
        fprintf(file, "\n");
    }

    double best_dist = DBL_MAX;
    int best_pos = -1;
    int best_cluster = -1;
    int best_i = -1;  // Store the original position of the cluster being moved
    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    memcpy(temp_order, cleaned_s0_order, cleaned_size * sizeof(int));

    // Swap between positions 1 and size-2
    for (int i = 1; i < cleaned_size - 1; ++i) {
        int cluster_to_move = cleaned_s0_order[i];
        if (cluster_to_move == 1) continue;

        for (int pos = 1; pos < cleaned_size - 1; ++pos) {
            if (pos == i) continue;

            // Create new order with swap
            int *new_order = malloc(MAX_DIMENSION * sizeof(int));
            memcpy(new_order, cleaned_s0_order, cleaned_size * sizeof(int));
            
            // Remove cluster from original position
            memmove(&new_order[i], &new_order[i + 1], (cleaned_size - i - 1) * sizeof(int));
            // Insert cluster at new position
            memmove(&new_order[pos + 1], &new_order[pos], (cleaned_size - pos - 1) * sizeof(int));
            new_order[pos] = cluster_to_move;

            // Validate: exactly two cluster 1s
            int cluster1_count = 0;
            for (int j = 0; j < cleaned_size; ++j) {
                if (new_order[j] == 1) cluster1_count++;
            }
            if (cluster1_count != 2 || new_order[0] != 1 || new_order[cleaned_size - 1] != 1) {
                free(new_order);
                continue;
            }

            double new_dist = calculateTotalDistance(dataset, dataset->clusters, new_order, cleaned_size);
            if (new_dist < best_dist && new_dist <= dataset->tmax) {
                best_dist = new_dist;
                best_pos = pos;
                best_cluster = cluster_to_move;
                best_i = i;
                memcpy(temp_order, new_order, cleaned_size * sizeof(int));
            }
            free(new_order);
        }
    }

    if (best_dist < s0_dist && best_cluster != -1) {
        // Apply the best swap explicitly
        memcpy(temp_order, cleaned_s0_order, cleaned_size * sizeof(int));
        memmove(&temp_order[best_i], &temp_order[best_i + 1], (cleaned_size - best_i - 1) * sizeof(int));
        memmove(&temp_order[best_pos + 1], &temp_order[best_pos], (cleaned_size - best_pos - 1) * sizeof(int));
        temp_order[best_pos] = best_cluster;

        *s5_order = realloc(*s5_order, cleaned_size * sizeof(int));
        memcpy(*s5_order, temp_order, cleaned_size * sizeof(int));
        *s5_size = cleaned_size;

        fprintf(file, "S5 Solution (Swap with clusters):\n");
        fprintf(file, "Swapped cluster %d from position %d to position %d\n", best_cluster, best_i, best_pos);
        printSolution(file, dataset, *s5_order, *s5_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s5_order, *s5_size);

        tryInsertion(dataset, s5_order, s5_size, &best_dist, &s0_prize, file);

        // Clean after insertion
        int *final_order = malloc(MAX_DIMENSION * sizeof(int));
        int final_size = 0;
        final_order[0] = 1;
        final_size++;
        for (int i = 1; i < *s5_size - 1; ++i) {
            if ((*s5_order)[i] != 1) {
                final_order[final_size++] = (*s5_order)[i];
            }
        }
        final_order[final_size++] = 1;

        if (final_size != *s5_size) {
            fprintf(file, "S5 Warning: Insertion added extra cluster 1, corrected\n");
        }

        *s5_order = realloc(*s5_order, final_size * sizeof(int));
        memcpy(*s5_order, final_order, final_size * sizeof(int));
        *s5_size = final_size;
        best_dist = calculateTotalDistance(dataset, dataset->clusters, *s5_order, *s5_size);
        s0_prize = calculateTotalPrize(dataset, *s5_order, *s5_size);

        fprintf(file, "S5 Solution after possible insertion:\n");
        printSolution(file, dataset, *s5_order, *s5_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s5_order, *s5_size);

        free(final_order);
    } else {
        *s5_order = realloc(*s5_order, cleaned_size * sizeof(int));
        memcpy(*s5_order, cleaned_s0_order, cleaned_size * sizeof(int));
        *s5_size = cleaned_size;
        fprintf(file, "S5 Solution: No valid swap with clusters found\n\n");
    }

    free(temp_order);
    free(cleaned_s0_order);
}

void evaluateAndApplyBestCluster2Opt(Dataset *dataset, int *s0_order, int s0_size, int **s6_order, int *s6_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    double best_dist = DBL_MAX;
    int best_i = -1;
    int best_j = -1;

    int *temp_order = malloc(s0_size * sizeof(int));
    memcpy(temp_order, s0_order, s0_size * sizeof(int));

    // Verify initial order has cluster 1 only at start and end
    if (s0_order[0] != 1 || s0_order[s0_size - 1] != 1) {
        fprintf(file, "S6 Error: Initial order does not start and end with cluster 1\n");
        *s6_order = realloc(*s6_order, s0_size * sizeof(int));
        memcpy(*s6_order, s0_order, s0_size * sizeof(int));
        *s6_size = s0_size;
        free(temp_order);
        return;
    }

    // Only consider segments between position 1 and size-2 (excluding start and end)
    for (int i = 1; i < s0_size - 2; ++i) {
        for (int j = i + 1; j < s0_size - 1; ++j) {
            memcpy(temp_order, s0_order, s0_size * sizeof(int));
            for (int left = i, right = j; left < right; ++left, --right) {
                int temp = temp_order[left];
                temp_order[left] = temp_order[right];
                temp_order[right] = temp;
            }

            // Strict validation: cluster 1 must be only at start and end
            if (temp_order[0] != 1 || temp_order[s0_size - 1] != 1) {
                continue;
            }
            bool valid = true;
            for (int k = 1; k < s0_size - 1; ++k) {
                if (temp_order[k] == 1) {
                    valid = false;
                    break;
                }
            }
            if (!valid) continue;

            double new_dist = calculateTotalDistance(dataset, dataset->clusters, temp_order, s0_size);
            if (new_dist < best_dist && new_dist <= dataset->tmax) {
                best_dist = new_dist;
                best_i = i;
                best_j = j;
            }
        }
    }

    if (best_dist < s0_dist && best_i != -1) {
        // Apply the best 2-opt move to the final order
        memcpy(temp_order, s0_order, s0_size * sizeof(int));
        for (int left = best_i, right = best_j; left < right; ++left, --right) {
            int temp = temp_order[left];
            temp_order[left] = temp_order[right];
            temp_order[right] = temp;
        }

        *s6_order = realloc(*s6_order, s0_size * sizeof(int));
        memcpy(*s6_order, temp_order, s0_size * sizeof(int));
        *s6_size = s0_size;

        fprintf(file, "S6 Solution (2-opt with clusters):\n");
        fprintf(file, "2-opt between positions %d and %d\n", best_i, best_j);
        printSolution(file, dataset, *s6_order, *s6_size, best_dist, s0_prize);
        printClusterDetails(file, dataset, *s6_order, *s6_size);

        tryInsertion(dataset, s6_order, s6_size, &best_dist, &s0_prize, file);
        
        // Validate after insertion
        if ((*s6_order)[0] != 1 || (*s6_order)[*s6_size - 1] != 1) {
            fprintf(file, "S6 Error: Cluster 1 not at start or end after insertion\n");
            *s6_order = realloc(*s6_order, s0_size * sizeof(int));
            memcpy(*s6_order, s0_order, s0_size * sizeof(int));
            *s6_size = s0_size;
        } else {
            fprintf(file, "S6 Solution after possible insertion:\n");
            printSolution(file, dataset, *s6_order, *s6_size, best_dist, s0_prize);
            printClusterDetails(file, dataset, *s6_order, *s6_size);
        }
    } else {
        *s6_order = realloc(*s6_order, s0_size * sizeof(int));
        memcpy(*s6_order, s0_order, s0_size * sizeof(int));
        *s6_size = s0_size;
        fprintf(file, "S6 Solution: No valid 2-opt with clusters found\n\n");
    }
    free(temp_order);
}

double calculatePd(int iter, int max_iter) {
    return 0.6 - 0.4 * ((double)iter / max_iter);
}

void evaluateAndApplyClusterRelocation(Dataset *dataset, int *s0_order, int s0_size, int **s7_order, int *s7_size, FILE *file, int iter) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    double pd = calculatePd(iter, MAX_NO_IMPROVEMENT_ITERATIONS);

    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    int temp_size = 0;
    int *removed_clusters = malloc(MAX_DIMENSION * sizeof(int));
    int removed_count = 0;

    temp_order[temp_size++] = 1;
    for (int i = 1; i < s0_size - 1; ++i) {
        if ((float)rand() / RAND_MAX < pd) {
            removed_clusters[removed_count++] = s0_order[i];
        } else {
            temp_order[temp_size++] = s0_order[i];
        }
    }
    temp_order[temp_size++] = 1;

    bool visited[MAX_DIMENSION] = {false};
    for (int i = 0; i < temp_size; ++i) visited[temp_order[i] - 1] = true;
    Cluster *unvisited_clusters = malloc(dataset->set_count * sizeof(Cluster));
    int unvisited_count = 0;
    for (int i = 0; i < dataset->set_count; ++i) {
        if (!visited[i]) unvisited_clusters[unvisited_count++] = dataset->clusters[i];
    }
    qsort(unvisited_clusters, unvisited_count, sizeof(Cluster), compareClustersByProfit);

    double current_dist = calculateTotalDistance(dataset, dataset->clusters, temp_order, temp_size);
    int current_prize = calculateTotalPrize(dataset, temp_order, temp_size);
    bool improved = false;

    for (int r = 0; r < unvisited_count && current_dist < dataset->tmax; ++r) {
        int cluster_id = unvisited_clusters[r].id;
        int best_pos = -1;
        double best_dist = DBL_MAX;
        for (int pos = 1; pos < temp_size; ++pos) {
            int *new_order = malloc((temp_size + 1) * sizeof(int));
            memcpy(new_order, temp_order, temp_size * sizeof(int));
            memmove(&new_order[pos + 1], &new_order[pos], (temp_size - pos) * sizeof(int));
            new_order[pos] = cluster_id;
            double new_dist = calculateTotalDistance(dataset, dataset->clusters, new_order, temp_size + 1);
            if (new_dist <= dataset->tmax && new_dist < best_dist) {
                best_dist = new_dist;
                best_pos = pos;
            }
            free(new_order);
        }
        if (best_pos != -1) {
            temp_size++;
            temp_order = realloc(temp_order, temp_size * sizeof(int));
            memmove(&temp_order[best_pos + 1], &temp_order[best_pos], (temp_size - best_pos - 1) * sizeof(int));
            temp_order[best_pos] = cluster_id;
            current_dist = best_dist;
            current_prize += unvisited_clusters[r].total_prize;
            improved = true;
        }
    }

    *s7_order = realloc(*s7_order, temp_size * sizeof(int));
    memcpy(*s7_order, temp_order, temp_size * sizeof(int));
    *s7_size = temp_size;

    if (improved && (current_prize > s0_prize || (current_prize == s0_prize && current_dist < s0_dist))) {
        fprintf(file, "S7 Solution (Cluster Relocation):\n");
        printSolution(file, dataset, *s7_order, *s7_size, current_dist, current_prize);
        printClusterDetails(file, dataset, *s7_order, *s7_size);

        tryInsertion(dataset, s7_order, s7_size, &current_dist, &current_prize, file);
        fprintf(file, "S7 Solution after possible insertion:\n");
        printSolution(file, dataset, *s7_order, *s7_size, current_dist, current_prize);
        printClusterDetails(file, dataset, *s7_order, *s7_size);
    } else {
        *s7_order = realloc(*s7_order, s0_size * sizeof(int));
        memcpy(*s7_order, s0_order, s0_size * sizeof(int));
        *s7_size = s0_size;
        fprintf(file, "S7 Solution: No improvement from cluster relocation\n\n");
    }

    free(temp_order);
    free(removed_clusters);
    free(unvisited_clusters);
}

void evaluateAndApplyClusterExchange(Dataset *dataset, int *s0_order, int s0_size, int **s8_order, int *s8_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);

    // Identify visited and unvisited clusters
    bool visited[MAX_DIMENSION] = {false};
    for (int i = 0; i < s0_size; ++i) {
        visited[s0_order[i] - 1] = true;
    }
    Cluster *unvisited_clusters = malloc(dataset->set_count * sizeof(Cluster));
    int unvisited_count = 0;
    for (int i = 0; i < dataset->set_count; ++i) {
        if (!visited[i]) {
            unvisited_clusters[unvisited_count] = dataset->clusters[i];
            unvisited_count++;
        }
    }
    qsort(unvisited_clusters, unvisited_count, sizeof(Cluster), compareClustersByPdRatio);

    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    memcpy(temp_order, s0_order, s0_size * sizeof(int));
    int temp_size = s0_size;

    double best_dist = s0_dist;
    int best_prize = s0_prize;
    int *best_order = malloc(MAX_DIMENSION * sizeof(int));
    memcpy(best_order, s0_order, s0_size * sizeof(int));
    int best_size = s0_size;
    bool improved = false;

    for (int pos = 1; pos < s0_size - 1; ++pos) {
        int cluster_to_remove = s0_order[pos];
        if (cluster_to_remove == 1) continue;

        for (int u = 0; u < unvisited_count; ++u) {
            int cluster_to_insert = unvisited_clusters[u].id;

            int *new_order = malloc(MAX_DIMENSION * sizeof(int));
            memcpy(new_order, s0_order, s0_size * sizeof(int));
            new_order[pos] = cluster_to_insert;

            bool has_duplicate = false;
            bool temp_visited[MAX_DIMENSION] = {false};
            for (int i = 0; i < s0_size; ++i) {
                if (i != 0 && i != s0_size - 1 && temp_visited[new_order[i] - 1]) {
                    has_duplicate = true;
                    break;
                }
                temp_visited[new_order[i] - 1] = true;
            }
            if (has_duplicate) {
                free(new_order);
                continue;
            }

            double new_dist = calculateTotalDistance(dataset, dataset->clusters, new_order, s0_size);
            int new_prize = calculateTotalPrize(dataset, new_order, s0_size);

            if (new_dist <= dataset->tmax && 
                (new_prize > best_prize || 
                 (new_prize == best_prize && new_dist < best_dist))) {
                best_dist = new_dist;
                best_prize = new_prize;
                memcpy(best_order, new_order, s0_size * sizeof(int));
                best_size = s0_size;
                improved = true;
                fprintf(file, "S8: Considering exchange of cluster %d with %d (p/d: %.2f) at pos %d (dist: %.2f, prize: %d)\n",
                        cluster_to_remove, cluster_to_insert, unvisited_clusters[u].pd_ratio, pos, new_dist, new_prize);
            }
            free(new_order);
        }
    }

    if (improved) {
        memcpy(temp_order, best_order, best_size * sizeof(int));
        temp_size = best_size;
        *s8_order = realloc(*s8_order, temp_size * sizeof(int));
        memcpy(*s8_order, temp_order, temp_size * sizeof(int));
        *s8_size = temp_size;

        fprintf(file, "S8 Solution (Cluster Exchange with High P/D Unvisited):\n");
        printSolution(file, dataset, *s8_order, *s8_size, best_dist, best_prize);
        printClusterDetails(file, dataset, *s8_order, *s8_size);

        tryInsertion(dataset, s8_order, s8_size, &best_dist, &best_prize, file);
        fprintf(file, "S8 Solution after possible insertion:\n");
        printSolution(file, dataset, *s8_order, *s8_size, best_dist, best_prize);
        printClusterDetails(file, dataset, *s8_order, *s8_size);
    } else {
        *s8_order = realloc(*s8_order, s0_size * sizeof(int));
        memcpy(*s8_order, s0_order, s0_size * sizeof(int));
        *s8_size = s0_size;
        fprintf(file, "S8 Solution: No improvement from cluster exchange with high p/d unvisited\n\n");
    }

    free(temp_order);
    free(best_order);
    free(unvisited_clusters);
}

void evaluateAndApplyRandomCitiesRemoval(Dataset *dataset, int *s0_order, int s0_size, int **s9_order, int *s9_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    bool improved = false;

    Cluster temp_clusters[MAX_DIMENSION];
    memcpy(temp_clusters, dataset->clusters, dataset->set_count * sizeof(Cluster));

    for (int k = 0; k < s0_size; ++k) {
        int cluster_idx = s0_order[k] - 1;
        Cluster *cluster = &temp_clusters[cluster_idx];
        if (cluster->size < 4) continue;

        int start_pos = rand() % (cluster->size - 2);
        int removed_vertices[3];
        for (int i = 0; i < 3; ++i) {
            removed_vertices[i] = cluster->best_order[start_pos + i];
        }
        int new_size = cluster->size - 3;
        int *temp_order = malloc(new_size * sizeof(int));
        int temp_idx = 0;
        for (int i = 0; i < cluster->size; ++i) {
            if (i < start_pos || i >= start_pos + 3) {
                temp_order[temp_idx++] = cluster->best_order[i];
            }
        }

        int best_pos = -1;
        double min_dist = DBL_MAX;
        int *new_order = malloc(cluster->size * sizeof(int));
        for (int pos = 0; pos <= new_size; ++pos) {
            int idx = 0;
            for (int i = 0; i < pos; ++i) new_order[idx++] = temp_order[i];
            for (int i = 0; i < 3; ++i) new_order[idx++] = removed_vertices[i];
            for (int i = pos; i < new_size; ++i) new_order[idx++] = temp_order[i];
            double new_cluster_dist = calculateClusterDistanceWithOrder(dataset, cluster, new_order);
            if (new_cluster_dist < min_dist) {
                min_dist = new_cluster_dist;
                best_pos = pos;
            }
        }

        int idx = 0;
        for (int i = 0; i < best_pos; ++i) cluster->best_order[idx++] = temp_order[i];
        for (int i = 0; i < 3; ++i) cluster->best_order[idx++] = removed_vertices[i];
        for (int i = best_pos; i < new_size; ++i) cluster->best_order[idx++] = temp_order[i];
        cluster->total_distance = min_dist;

        free(temp_order);
        free(new_order);
    }

    double new_total_dist = calculateTotalDistance(dataset, temp_clusters, s0_order, s0_size);
    if (new_total_dist < s0_dist && new_total_dist <= dataset->tmax) {
        for (int i = 0; i < dataset->set_count; ++i) {
            memcpy(dataset->clusters[i].best_order, temp_clusters[i].best_order, dataset->clusters[i].size * sizeof(int));
            dataset->clusters[i].total_distance = temp_clusters[i].total_distance;
        }
        improved = true;
    }

    *s9_order = realloc(*s9_order, s0_size * sizeof(int));
    memcpy(*s9_order, s0_order, s0_size * sizeof(int));
    *s9_size = s0_size;

    if (improved) {
        fprintf(file, "S9 Solution (Random Cities Removal):\n");
        printSolution(file, dataset, *s9_order, *s9_size, new_total_dist, s0_prize);
        printClusterDetails(file, dataset, *s9_order, *s9_size);

        tryInsertion(dataset, s9_order, s9_size, &new_total_dist, &s0_prize, file);
        fprintf(file, "S9 Solution after possible insertion:\n");
        printSolution(file, dataset, *s9_order, *s9_size, new_total_dist, s0_prize);
        printClusterDetails(file, dataset, *s9_order, *s9_size);
    } else {
        fprintf(file, "S9 Solution: No improvement from random cities removal\n\n");
    }
}

void evaluateAndApplyRandomClusterRemoval(Dataset *dataset, int *s0_order, int s0_size, int **s10_order, int *s10_size, FILE *file) {
    double s0_dist = calculateTotalDistance(dataset, dataset->clusters, s0_order, s0_size);
    int s0_prize = calculateTotalPrize(dataset, s0_order, s0_size);
    if (s0_size < 5) {
        *s10_order = realloc(*s10_order, s0_size * sizeof(int));
        memcpy(*s10_order, s0_order, s0_size * sizeof(int));
        *s10_size = s0_size;
        fprintf(file, "S10 Solution: Not enough clusters to remove\n\n");
        return;
    }

    int start_pos = 1 + (rand() % (s0_size - 4));
    int removed_clusters[3];
    for (int i = 0; i < 3; ++i) {
        removed_clusters[i] = s0_order[start_pos + i];
    }
    int temp_size = s0_size - 3;
    int *temp_order = malloc(MAX_DIMENSION * sizeof(int));
    int temp_idx = 0;
    for (int i = 0; i < s0_size; ++i) {
        if (i < start_pos || i >= start_pos + 3) {
            temp_order[temp_idx++] = s0_order[i];
        }
    }

    bool visited[MAX_DIMENSION] = {false};
    for (int i = 0; i < temp_size; ++i) visited[temp_order[i] - 1] = true;
    Cluster *unvisited_clusters = malloc(dataset->set_count * sizeof(Cluster));
    int unvisited_count = 0;
    for (int i = 0; i < dataset->set_count; ++i) {
        if (!visited[i]) unvisited_clusters[unvisited_count++] = dataset->clusters[i];
    }
    qsort(unvisited_clusters, unvisited_count, sizeof(Cluster), compareClustersByProfit);

    double current_dist = calculateTotalDistance(dataset, dataset->clusters, temp_order, temp_size);
    int current_prize = calculateTotalPrize(dataset, temp_order, temp_size);
    bool improved = false;

    for (int r = 0; r < unvisited_count && current_dist < dataset->tmax; ++r) {
        int cluster_id = unvisited_clusters[r].id;
        int best_pos = -1;
        double best_dist = DBL_MAX;
        int best_prize = current_prize;

        for (int pos = 1; pos < temp_size; ++pos) {
            int *new_order = malloc((temp_size + 1) * sizeof(int));
            memcpy(new_order, temp_order, temp_size * sizeof(int));
            memmove(&new_order[pos + 1], &new_order[pos], (temp_size - pos) * sizeof(int));
            new_order[pos] = cluster_id;

            double new_dist = calculateTotalDistance(dataset, dataset->clusters, new_order, temp_size + 1);
            int new_prize = calculateTotalPrize(dataset, new_order, temp_size + 1);

            if (new_dist <= dataset->tmax && (new_prize > best_prize || (new_prize == best_prize && new_dist < best_dist))) {
                best_dist = new_dist;
                best_prize = new_prize;
                best_pos = pos;
            }
            free(new_order);
        }

        if (best_pos != -1) {
            temp_size++;
            temp_order = realloc(temp_order, temp_size * sizeof(int));
            memmove(&temp_order[best_pos + 1], &temp_order[best_pos], (temp_size - best_pos - 1) * sizeof(int));
            temp_order[best_pos] = cluster_id;
            current_dist = best_dist;
            current_prize = best_prize;
            improved = true;
        }
    }

    *s10_order = realloc(*s10_order, temp_size * sizeof(int));
    memcpy(*s10_order, temp_order, temp_size * sizeof(int));
    *s10_size = temp_size;

    if (improved && (current_prize > s0_prize || (current_prize == s0_prize && current_dist < s0_dist))) {
        fprintf(file, "S10 Solution (Random Cluster Removal):\n");
        printSolution(file, dataset, *s10_order, *s10_size, current_dist, current_prize);
        printClusterDetails(file, dataset, *s10_order, *s10_size);

        tryInsertion(dataset, s10_order, s10_size, &current_dist, &current_prize, file);
        fprintf(file, "S10 Solution after possible insertion:\n");
        printSolution(file, dataset, *s10_order, *s10_size, current_dist, current_prize);
        printClusterDetails(file, dataset, *s10_order, *s10_size);
    } else {
        *s10_order = realloc(*s10_order, s0_size * sizeof(int));
        memcpy(*s10_order, s0_order, s0_size * sizeof(int));
        *s10_size = s0_size;
        fprintf(file, "S10 Solution: No improvement from random cluster removal\n\n");
    }

    free(temp_order);
    free(unvisited_clusters);
}

int main(int argc, char *argv[]) {
    printf("Program started\n");
    if (argc < 2) {
        printf("Usage: %s <dataset_file>\n", argv[0]);
        return 1;
    }

    Dataset *dataset = malloc(sizeof(Dataset));
    if (!dataset) {
        perror("Memory allocation failed for dataset");
        exit(EXIT_FAILURE);
    }
    memset(dataset, 0, sizeof(Dataset));
    readDataset(dataset, argv[1]);

    FILE *output_file = fopen("best_profit.txt", "w");
    FILE *temp_file = fopen("temp.txt", "w");
    if (!output_file || !temp_file) {
        perror("Error opening output files");
        if (dataset) {
            for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
            free(dataset->edge_weight);
            free(dataset);
        }
        if (output_file) fclose(output_file);
        if (temp_file) fclose(temp_file);
        return EXIT_FAILURE;
    }
    fprintf(temp_file, "Files opened successfully\n");
    fflush(temp_file);

    for (int i = 0; i < dataset->set_count; ++i) {
        kMedoids(dataset, &dataset->clusters[i]);
    }

    // Create infeasible solution
    int *infeasible_order = malloc(MAX_DIMENSION * sizeof(int));
    if (!infeasible_order) {
        perror("Memory allocation failed for infeasible_order");
        for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
        free(dataset->edge_weight);
        free(dataset);
        fclose(output_file);
        fclose(temp_file);
        exit(EXIT_FAILURE);
    }
    int infeasible_size = 2;  // Start with 1-1
    infeasible_order[0] = 1;  // Starting with cluster 1
    infeasible_order[1] = 1;  // Ending with cluster 1

    Cluster *sorted_clusters = malloc(dataset->set_count * sizeof(Cluster));
    if (!sorted_clusters) {
        perror("Memory allocation failed for sorted_clusters");
        free(infeasible_order);
        for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
        free(dataset->edge_weight);
        free(dataset);
        fclose(output_file);
        fclose(temp_file);
        exit(EXIT_FAILURE);
    }
    memcpy(sorted_clusters, dataset->clusters, dataset->set_count * sizeof(Cluster));
    qsort(sorted_clusters, dataset->set_count, sizeof(Cluster), compareClustersByPdRatio);

    // Insert clusters based on highest p/d ratio
    bool visited[MAX_DIMENSION] = {false};
    visited[0] = true;  // Cluster 1 is already included
    
    for (int i = 0; i < dataset->set_count; i++) {
        int cluster_id = sorted_clusters[i].id;
        if (cluster_id == 1 || visited[cluster_id - 1]) continue;

        double best_dist = DBL_MAX;
        int best_pos = -1;
        int *best_order = malloc((infeasible_size + 1) * sizeof(int));

        // Try inserting at each possible position between start and end
        for (int pos = 1; pos < infeasible_size; pos++) {
            int *temp_order = malloc((infeasible_size + 1) * sizeof(int));
            memcpy(temp_order, infeasible_order, infeasible_size * sizeof(int));
            memmove(&temp_order[pos + 1], &temp_order[pos], (infeasible_size - pos) * sizeof(int));
            temp_order[pos] = cluster_id;

            double temp_dist = calculateTotalDistance(dataset, dataset->clusters, temp_order, infeasible_size + 1);
            if (temp_dist < best_dist) {
                best_dist = temp_dist;
                best_pos = pos;
                memcpy(best_order, temp_order, (infeasible_size + 1) * sizeof(int));
            }
            free(temp_order);
        }

        // Update the infeasible order with the best position found
        infeasible_order = realloc(infeasible_order, (infeasible_size + 1) * sizeof(int));
        memcpy(infeasible_order, best_order, (infeasible_size + 1) * sizeof(int));
        infeasible_size++;
        visited[cluster_id - 1] = true;
        free(best_order);
    }

    double infeasible_dist = calculateTotalDistance(dataset, dataset->clusters, infeasible_order, infeasible_size);
    int infeasible_prize = calculateTotalPrize(dataset, infeasible_order, infeasible_size);
    fprintf(temp_file, "Infeasible Solution:\n");
    printSolution(temp_file, dataset, infeasible_order, infeasible_size, infeasible_dist, infeasible_prize);
    printClusterDetails(temp_file, dataset, infeasible_order, infeasible_size);
    fprintf(temp_file, "Infeasible Distance: %.2f, tmax: %d\n", infeasible_dist, dataset->tmax);
    fflush(temp_file);

    // Create S0 solution
    int *s0_order = NULL;
    int s0_size = 0;
    double s0_dist = 0;
    int s0_prize = 0;
    makeS0Solution(dataset, infeasible_order, infeasible_size, &s0_order, &s0_size, &s0_dist, &s0_prize);
    fprintf(temp_file, "S0 Solution (Feasible Solution):\n");
    printSolution(temp_file, dataset, s0_order, s0_size, s0_dist, s0_prize);
    printClusterDetails(temp_file, dataset, s0_order, s0_size);
    fprintf(temp_file, "S0 Distance: %.2f, tmax: %d\n", s0_dist, dataset->tmax);
    fflush(temp_file);

    double overall_best_dist = DBL_MAX;
    int overall_best_prize = 0;
    int *overall_best_order = malloc(MAX_DIMENSION * sizeof(int));
    if (!overall_best_order) {
        perror("Memory allocation failed for overall_best_order");
        free(infeasible_order);
        free(sorted_clusters);
        free(s0_order);
        for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
        free(dataset->edge_weight);
        free(dataset);
        fclose(output_file);
        fclose(temp_file);
        exit(EXIT_FAILURE);
    }
    int overall_best_size = 0;

    // Initialize overall best with S0 if feasible
    if (s0_dist <= dataset->tmax) {
        overall_best_dist = s0_dist;
        overall_best_prize = s0_prize;
        memcpy(overall_best_order, s0_order, s0_size * sizeof(int));
        overall_best_size = s0_size;
    }

    clock_t total_start = clock();
    double avg_prize = 0.0;
    double total_time_per_run[NO_RUNS];

    int *s_orders[11];
    int s_sizes[11];
    double s_dists[11];
    int s_prizes[11];

    for (int run = 1; run <= NO_RUNS; ++run) {
        printf("Starting run %d\n", run);
        srand(run);
        fprintf(temp_file, "run = %d:\n", run);
        fflush(temp_file);
        clock_t run_start = clock();

        double run_best_dist = s0_dist;
        int run_best_prize = s0_prize;
        int *run_best_order = malloc(s0_size * sizeof(int));
        if (!run_best_order) {
            perror("Memory allocation failed for run_best_order");
            free(infeasible_order);
            free(sorted_clusters);
            free(s0_order);
            free(overall_best_order);
            for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
            free(dataset->edge_weight);
            free(dataset);
            fclose(output_file);
            fclose(temp_file);
            exit(EXIT_FAILURE);
        }
        int run_best_size = s0_size;
        memcpy(run_best_order, s0_order, s0_size * sizeof(int));

        for (int restart = 1; restart <= NO_RESTARTS; ++restart) {
            fprintf(temp_file, "restart = %d:\n", restart);
            fflush(temp_file);

            s_orders[0] = malloc(s0_size * sizeof(int));
            if (!s_orders[0]) {
                perror("Memory allocation failed for s_orders[0]");
                free(infeasible_order);
                free(sorted_clusters);
                free(s0_order);
                free(overall_best_order);
                free(run_best_order);
                for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
                free(dataset->edge_weight);
                free(dataset);
                fclose(output_file);
                fclose(temp_file);
                exit(EXIT_FAILURE);
            }
            memcpy(s_orders[0], s0_order, s0_size * sizeof(int));
            s_sizes[0] = s0_size;
            s_dists[0] = s0_dist;
            s_prizes[0] = s0_prize;

            int iter = 0;
            int no_imp = 0;

            while (no_imp < MAX_NO_IMPROVEMENT_ITERATIONS) {
                printf("Run %d, Restart %d, Iteration %d, no_imp = %d\n", run, restart, iter + 1, no_imp);
                fprintf(temp_file, "iteration = %d:\n\n", iter + 1);
                fflush(temp_file);

                for (int i = 1; i < 11; ++i) {
                    s_orders[i] = NULL;
                }

                evaluateAndApplyBestInsertion(dataset, s_orders[0], s_sizes[0], &s_orders[1], &s_sizes[1], temp_file);
                s_dists[1] = calculateTotalDistance(dataset, dataset->clusters, s_orders[1], s_sizes[1]);
                s_prizes[1] = calculateTotalPrize(dataset, s_orders[1], s_sizes[1]);

                evaluateAndApplyBestSwap(dataset, s_orders[0], s_sizes[0], &s_orders[2], &s_sizes[2], temp_file);
                s_dists[2] = calculateTotalDistance(dataset, dataset->clusters, s_orders[2], s_sizes[2]);
                s_prizes[2] = calculateTotalPrize(dataset, s_orders[2], s_sizes[2]);

                evaluateAndApplyBest2Opt(dataset, s_orders[0], s_sizes[0], &s_orders[3], &s_sizes[3], temp_file);
                s_dists[3] = calculateTotalDistance(dataset, dataset->clusters, s_orders[3], s_sizes[3]);
                s_prizes[3] = calculateTotalPrize(dataset, s_orders[3], s_sizes[3]);

                evaluateAndApplyBestRelocation(dataset, s_orders[0], s_sizes[0], &s_orders[4], &s_sizes[4], temp_file);
                s_dists[4] = calculateTotalDistance(dataset, dataset->clusters, s_orders[4], s_sizes[4]);
                s_prizes[4] = calculateTotalPrize(dataset, s_orders[4], s_sizes[4]);

                evaluateAndApplyBestClusterSwap(dataset, s_orders[0], s_sizes[0], &s_orders[5], &s_sizes[5], temp_file);
                s_dists[5] = calculateTotalDistance(dataset, dataset->clusters, s_orders[5], s_sizes[5]);
                s_prizes[5] = calculateTotalPrize(dataset, s_orders[5], s_sizes[5]);

                evaluateAndApplyBestCluster2Opt(dataset, s_orders[0], s_sizes[0], &s_orders[6], &s_sizes[6], temp_file);
                s_dists[6] = calculateTotalDistance(dataset, dataset->clusters, s_orders[6], s_sizes[6]);
                s_prizes[6] = calculateTotalPrize(dataset, s_orders[6], s_sizes[6]);

                evaluateAndApplyClusterRelocation(dataset, s_orders[0], s_sizes[0], &s_orders[7], &s_sizes[7], temp_file, iter);
                s_dists[7] = calculateTotalDistance(dataset, dataset->clusters, s_orders[7], s_sizes[7]);
                s_prizes[7] = calculateTotalPrize(dataset, s_orders[7], s_sizes[7]);

                evaluateAndApplyClusterExchange(dataset, s_orders[0], s_sizes[0], &s_orders[8], &s_sizes[8], temp_file);
                s_dists[8] = calculateTotalDistance(dataset, dataset->clusters, s_orders[8], s_sizes[8]);
                s_prizes[8] = calculateTotalPrize(dataset, s_orders[8], s_sizes[8]);

                evaluateAndApplyRandomCitiesRemoval(dataset, s_orders[0], s_sizes[0], &s_orders[9], &s_sizes[9], temp_file);
                s_dists[9] = calculateTotalDistance(dataset, dataset->clusters, s_orders[9], s_sizes[9]);
                s_prizes[9] = calculateTotalPrize(dataset, s_orders[9], s_sizes[9]);

                evaluateAndApplyRandomClusterRemoval(dataset, s_orders[0], s_sizes[0], &s_orders[10], &s_sizes[10], temp_file);
                s_dists[10] = calculateTotalDistance(dataset, dataset->clusters, s_orders[10], s_sizes[10]);
                s_prizes[10] = calculateTotalPrize(dataset, s_orders[10], s_sizes[10]);

                int best_operator = 0;
                for (int i = 1; i < 11; ++i) {
                    if (s_prizes[i] > s_prizes[best_operator] ||
                        (s_prizes[i] == s_prizes[best_operator] && s_dists[i] < s_dists[best_operator])) {
                        best_operator = i;
                    }
                }

                fprintf(temp_file, "Best operator: S%d, Prize: %d, Distance: %.2f\n", best_operator, s_prizes[best_operator], s_dists[best_operator]);
                fflush(temp_file);

                if (s_prizes[best_operator] > s_prizes[0] || 
                    (s_prizes[best_operator] == s_prizes[0] && s_dists[best_operator] < s_dists[0])) {
                    free(s_orders[0]);
                    s_orders[0] = malloc(s_sizes[best_operator] * sizeof(int));
                    if (!s_orders[0]) {
                        perror("Memory allocation failed for updated s_orders[0]");
                        for (int i = 1; i < 11; ++i) free(s_orders[i]);
                        free(infeasible_order);
                        free(sorted_clusters);
                        free(s0_order);
                        free(overall_best_order);
                        free(run_best_order);
                        for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
                        free(dataset->edge_weight);
                        free(dataset);
                        fclose(output_file);
                        fclose(temp_file);
                        exit(EXIT_FAILURE);
                    }
                    memcpy(s_orders[0], s_orders[best_operator], s_sizes[best_operator] * sizeof(int));
                    s_sizes[0] = s_sizes[best_operator];
                    s_dists[0] = s_dists[best_operator];
                    s_prizes[0] = s_prizes[best_operator];
                    no_imp = 0;

                    if (s_prizes[0] > run_best_prize || 
                        (s_prizes[0] == run_best_prize && s_dists[0] < run_best_dist)) {
                        run_best_dist = s_dists[0];
                        run_best_prize = s_prizes[0];
                        run_best_order = realloc(run_best_order, s_sizes[0] * sizeof(int));
                        memcpy(run_best_order, s_orders[0], s_sizes[0] * sizeof(int));
                        run_best_size = s_sizes[0];
                    }
                } else {
                    no_imp++;
                }

                enforceCluster1StartEnd(s_orders[0], &s_sizes[0]);
                for (int i = 1; i < 11; ++i) {
                    free(s_orders[i]);
                }

                iter++;
            }

            if (run_best_prize > overall_best_prize || 
                (run_best_prize == overall_best_prize && run_best_dist < overall_best_dist)) {
                overall_best_dist = run_best_dist;
                overall_best_prize = run_best_prize;
                overall_best_order = realloc(overall_best_order, run_best_size * sizeof(int));
                memcpy(overall_best_order, run_best_order, run_best_size * sizeof(int));
                overall_best_size = run_best_size;
            }

            free(s_orders[0]);
            s_orders[0] = malloc(run_best_size * sizeof(int));
            if (!s_orders[0]) {
                perror("Memory allocation failed for s_orders[0] restart");
                free(infeasible_order);
                free(sorted_clusters);
                free(s0_order);
                free (overall_best_order);
                free(run_best_order);
                for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
                free(dataset->edge_weight);
                free(dataset);
                fclose(output_file);
                fclose(temp_file);
                exit(EXIT_FAILURE);
            }
            memcpy(s_orders[0], run_best_order, run_best_size * sizeof(int));
            s_sizes[0] = run_best_size;
            s_dists[0] = run_best_dist;
            s_prizes[0] = run_best_prize;
        }

        clock_t run_end = clock();
        double run_time = (double)(run_end - run_start) / CLOCKS_PER_SEC;
        total_time_per_run[run - 1] = run_time;
        avg_prize += run_best_prize;

        fprintf(temp_file, "Run %d Best Solution:\n", run);
        printSolution(temp_file, dataset, run_best_order, run_best_size, run_best_dist, run_best_prize);
        printClusterDetails(temp_file, dataset, run_best_order, run_best_size);
        fprintf(temp_file, "Run %d Time: %.2f seconds\n\n", run, run_time);
        fflush(temp_file);

        free(run_best_order);
    }

    clock_t total_end = clock();
    double total_time = (double)(total_end - total_start) / CLOCKS_PER_SEC;
    avg_prize /= NO_RUNS;

    fprintf(temp_file, "Overall Best Solution:\n");
    printSolution(temp_file, dataset, overall_best_order, overall_best_size, overall_best_dist, overall_best_prize);
    printClusterDetails(temp_file, dataset, overall_best_order, overall_best_size);
    fprintf(temp_file, "Total Time: %.2f seconds\n", total_time);
    fprintf(temp_file, "Average Prize: %.2f\n", avg_prize);
    for (int i = 0; i < NO_RUNS; ++i) {
        fprintf(temp_file, "Run %d Time: %.2f seconds\n", i + 1, total_time_per_run[i]);
    }
    fflush(temp_file);

    fprintf(output_file, "Overall Best Solution:\n");
    printSolution(output_file, dataset, overall_best_order, overall_best_size, overall_best_dist, overall_best_prize);
    printClusterDetails(output_file, dataset, overall_best_order, overall_best_size);
    fflush(output_file);

    free(infeasible_order);
    free(sorted_clusters);
    free(s0_order);
    free(overall_best_order);
    for (int i = 0; i < dataset->dimension; ++i) free(dataset->edge_weight[i]);
    free(dataset->edge_weight);
    free(dataset);
    fclose(output_file);
    fclose(temp_file);

    printf("Program completed successfully\n");
    return 0;
}