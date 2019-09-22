import csv 

with open('iris.data') as f:
    reader = csv.reader(f)

    for row in reader:
        name = row[-1].split('-')[-1].title()
        row[-1] = name
        print(", {{ sepal = {{ length = Length.centimeters {}, width = Length.centimeters {} }}, petal = {{ length = Length.centimeters {}, width = Length.centimeters {} }}, class = {} }}".format(*row))

        
