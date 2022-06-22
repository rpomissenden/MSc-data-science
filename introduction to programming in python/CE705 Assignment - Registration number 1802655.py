
def load_from_csv(file_name):
    #Import CSV has been used to bring in the function to support the reading of the CSV file. 
    import csv
    #Try and except has been used to flag errors loading the CSV to the user as the filepath will change depending on the user
    try:
        #Read file 
        with open(file_name) as data_source:
            #calls function from import csv to read in rows as lists with quote_nonnumeric ensuring values from the file are floats rather than strings
            file = csv.reader(data_source, quoting=csv.QUOTE_NONNUMERIC)
            #puts the rows (that are now lists) into a list to create a matrix (list of a list)
            matrix = list(file)
            return matrix
    except:
        print("Unable to load CSV")

def get_distance(list1, list2):
    #length of each list must be the same for the Manhattan distance formula to work. if statement is used to catch the error and signpost the user to a solution
    if len(list1) != len(list2):
        print("Lists aren't the same length")
    else:
        #returns the Manhattan distance between two lists.
        #For loop iterates through the number of elements in list2 to allow i to access the relevant element/variable in list1 and list2.
        #As i iterates it calculates the manhattan distance between each value in the lists.
        return sum([abs(list1[i]-list2[i]) for i in range(len(list2))])
        

def get_max(matrix, col_num):
    #List comprehension has been used as a more concise way of transposing the matrix to access values by column instead of row
    column = [row[col_num] for row in matrix]
    #max is used to compute and return the maximum value of the specified column
    max_value = max(column)
    return max_value


def get_min(matrix, col_num):
    #List comprehension was used as a more concise way of transposing the matrix to access values by column instead of row
    column = [row[col_num] for row in matrix]
    #min is used to compute and return the minimum value of the specified column
    min_value = min(column)
    return min_value

def get_mean(matrix, col_num):
    #get_mean has been created to condense the lines of code in get_standardised_matrix
    #List comprehension was used as a more concise way of transposing the matrix to access values by column instead of row
    column = [row[col_num] for row in matrix]
    #mean value uses sum and len to compute and return the mean of the specified column
    mean_value = sum(column)/len(column)
    return mean_value


def get_standardised_matrix(matrix):
    #Empty list created ready to append standardised rows into a matrix
    standardised_matrix = []
    i = 0
    #iterate through matrix and then rows to get to the value
    for row in matrix:
        #each iteration in the matrix creates a blank list ready to append the standardised version of the rows
        standardised_row = []
        #column number becomes 0 through each iteration of the matrix so nested forloop below can use it to select each column value in that row
        col_num = 0
        for value in row:
            #Loop through each row to compute the standardised values using the data standardisation formula
            #each loop the numerator selects the row (using i) and column value (using col_num) subtracting get_mean which is called to get the mean of that column
            numerator = matrix[i][col_num] - get_mean(matrix, col_num)
            #denominator gets the maximum value of the specified column by calling get_max and subtracts the minimum value of the specified column by calling get_min
            denominator = get_max(matrix, col_num) - get_min(matrix, col_num)
            #numerator and denominator are divided to get the standardised value
            standardised_value = numerator / denominator
            #append each standardised value to the new standardised row
            standardised_row.append(standardised_value)
            #column number increases so the next loop looks at the next value in the row and the correct column for that value for get_max, get_min and get_mean
            col_num += 1
        #append each standardised row to form a standardised matrix
        standardised_matrix.append(standardised_row)
        #i increases by 1 each loop to select the next row from the matrix in numerator
        i += 1
    return standardised_matrix


def get_median(matrix, col_num):
    #List comprehension was used as a more concise way of transposing the matrix to access values by column instead of row
    column = [row[col_num] for row in matrix]
    n = len(column)
    #column has been sorted as part of the process to calculate the median
    sorted_column = sorted(column)
    #Splice selects the two middle values of the sorted column, sums them and divides by two for the first element
    #Second element divides n by 2 and selects that element from sorted column
    #Modulus has been used to find if the remainder of n is 0 or 1 and selects the calculation at the correct element depending on if N is divisible by 2
    #returns the median of the specified column number in the matrix
    return (sum(sorted_column[n//2-1:n//2+1])/2.0, sorted_column[n//2])[n % 2] if n else None

def get_centroids(matrix, S, K):
    #avoids trivial groups by only allowing values of K in the interval [2, N-1]
    if (K <= 1) or (K > len(matrix)-1):
        print("Number of groups ('K') must be between 2 and", len(matrix)-1)
    else:
        #creates K number of empty lists inside a list. Absolute value of K ensures only positive integers
        Cluster_k = [[] for _ in range(abs(K))]
        Count_Cluster_k = 0
        #size brings across the length of the rows in S to be used later to iterate through each column
        size = len(S[0][0])
        #loop through S and find the median of each column
        for cluster in S:
            #loop appropriate number of times based on length of the row
            for col_num in range(size):
                #calls get_median to find the median of each column in the cluster
                median = get_median(cluster, col_num)
                #appends the median of the column to the correct empty list inside Cluster_k
                Cluster_k[Count_Cluster_k].append(median)
            Count_Cluster_k +=1
        return Cluster_k

def get_groups(matrix, K):
    import random
    #avoids trivial groups by only allowing values of K in the interval [2, N-1]
    if (K <= 1) or (K > len(matrix)-1):
        print("Number of groups ('K') must be between 2 and", len(matrix)-1)
    else:
        #create K number of empty lists. Absolute value of K ensures only positive integers
        S = [[] for _ in range(abs(K))]
        #pulls in the standardised matrix and assigns to a variable for more concise code
        standardised_matrix = get_standardised_matrix(matrix)
        #create K number of random rows from standardised matrix as initial starting points. Absolute value of K ensures only positive integers
        random_rows = random.sample(standardised_matrix, abs(K))
        #Starts each cluster as an empty list ready for appending
        cluster_k=[]
        #Starts old S as an empty list ready for new S to be appended at the end of the while loop if S doesn't equal OldS
        OldS = []

        #append each K row's from random_rows to cluster_k 
        for row in random_rows:
                    cluster_k.append(row)
        #while loop checks whether the old list S is equal to the newly computed list S
        while OldS != S:
            #iterate through standardised matrix
            for stand_row in standardised_matrix:
                cluster_count = 1
                man_distances=[]
                #iterates through the K number of rows in cluster_k
                for cluster_row in cluster_k:
                    #computes the Manhattan distance between each standardised row and the K number of rows in cluster_k 
                    man_distance = get_distance(cluster_row, stand_row)
                    #appends a list so it contains the manhattan distance, cluster number (e.g if K = 3, will be between 1-3) and row from the standardised matrix
                    man_distances.append([man_distance, cluster_count, stand_row])
                    cluster_count+=1
                #iterates through the manhattan distances, finds the minimum distance and nearest cluster
                for j in range(len(man_distances)):
                    if(min(man_distances)[0] == man_distances[j][0]):
                        #cluster number gets assigned to nearest cluster and relevant standardised row is appended to that cluster in S
                        nearest_cluster = man_distances[j][1]
                        S[nearest_cluster-1].append(stand_row)
            #while loop breaks when the list S no longer changes and returns list S to finish the function
            if OldS == S:
                break
            else:
                #if S does change then get_centroids is called and cluster_k gets replaced with the medians of the previous groupings
                cluster_k = get_centroids(standardised_matrix, S, K)
                #OldS gets emptied and new S is iterated through to append current S clusters into OldS for comparison
                OldS = []
                for x in S:
                    OldS.append(x)
                #Once S has now become OldS the new S gets populated with K number of empty lists to start the loop again
                #Absolute value of K ensures only positive integers
                S = [[] for _ in range(abs(K))]
        return S

def run_test():
    #defines the path of the CSV file
    path = '//Users//Ryan1//Documents//Data.csv'
    #calls the function to load the CSV file using the path provided
    Data = load_from_csv(path)
    #defines the number of experiments requested
    ExperimentNumbers = [2, 3, 4, 5, 6]
    #loops through each experiment number
    for K in ExperimentNumbers:
        print(" ")
        print("Experiment K = ", K)
        print(" ")
        #calls get_groups to run the K-Median algorithm using the CSV file and number of groups (K) specified by th experiment
        S = get_groups(Data, K)
        #Keeps count of the number for each group
        cluster_count = 1        
        #loops through S to print the length of each of the clusters to identify how many entities/wines have been assigned to each cluster/group
        for clusters in S:
            print("Group", cluster_count," - Number of entities (Wines):", len(clusters))
            cluster_count += 1
