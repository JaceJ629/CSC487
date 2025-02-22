def manhattan(v1, v2):
    dist = 0
    for i in range(len(v1)):
        dist += abs(v1[i] - v2[i])
    return dist

def euclidean(v1, v2):
    dist = 0
    for i in range(len(v1)):
        dist += (v1[i] - v2[i]) ** 2
    return dist ** 0.5

v1 = [float(x) for x in input("Enter the values for vector 1 separated by commas: ").split(",")]
v2 = [float(x) for x in input("Enter the values for vector 2 separated by commas: ").split(",")]

print("Manhattan Distance:", manhattan(v1, v2))
print("Euclidean Distance:", euclidean(v1, v2))