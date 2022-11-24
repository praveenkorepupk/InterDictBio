# InterDictBio
Inter Dictionary R shiny Application Development
Accessing the INTERDICTionary Application
![image](https://user-images.githubusercontent.com/79224911/203612746-91b65fdd-0d46-491b-b8d3-8289797d3a74.png)

Home page after logged in
![image](https://user-images.githubusercontent.com/79224911/203612765-a7b285e6-9f26-486c-ad4a-905bba946b03.png)
 
Query
a.	Normal
User can enter the n number of sequences with comma separated
i.	E.g. AAAA,AANA, AACA

b.	Regex
In order to search manually for multiple sequences using normal search, providing user to filter data using Regex so that user can filter multiple sequences data to analyse.
E.g. ^PP[G,A,V].$


	
Input box
After selected the query option, user can enter the sequence in the provided input box.

Filter by sequence
User can filter the data table using this action button.
	
Find overlap
User can click on this action button to find the overlapped sequences based on Entry.
UniProt
User can click on this UniProt logo, it redirects to UniProt website https://www.uniprot.org/id-mapping


In this application there are 4-tab panels
Results Summary
Shows the filtered data which is generated using query
 ![image](https://user-images.githubusercontent.com/79224911/203612817-feeb2ad5-ed84-4b18-8467-317bc59fe4c5.png)

Selected Data
•	Shows the selected rows which are selected from Results Summary, for this user must click on the Extract Data
![image](https://user-images.githubusercontent.com/79224911/203612849-375953fc-c1ae-4b8c-81b8-44c7928ae8bf.png)
 
•	Go to Selected Data tab-panel and click on Extract Data button then user can see the data like below
 ![image](https://user-images.githubusercontent.com/79224911/203612873-a111c5a6-fa03-4bf0-ab8b-7b35ba53413d.png)


Dashboard
•	Dashboard will generate the plots and summary counts based on filtered data
i.	Summary counts will be displayed automatically
ii.	To generate the plots user must click on the Generate Plots button
•	Dashboard will be changed dynamically after changed the query in Results Summary
![image](https://user-images.githubusercontent.com/79224911/203612963-943333ca-ff11-48a5-958d-c73be2e50ad8.png)
Sequence vs Entry
![image](https://user-images.githubusercontent.com/79224911/203613074-ad64cfcf-563a-4d9e-85a8-0802868a8199.png)

Sequence vs Count  
![image](https://user-images.githubusercontent.com/79224911/203613098-5ff2a657-01ec-4714-86e6-77625d069fd2.png)

Sequence vs Gene Count
![image](https://user-images.githubusercontent.com/79224911/203613121-41da86ab-e7ab-4c64-ac5e-e4ed38070236.png)
 
Admin
•	Admin tab-panel will be accessible only by admin
•	Admin panel will contain multiple options to manipulate the data which is presented in Selected Data tab-panel
![image](https://user-images.githubusercontent.com/79224911/203613143-8b19b784-b116-406c-89dd-0e79ea4ae3f6.png)

i.	Add Row
1.	Download the template and add data values to that template and upload it click on the Combine Rows button
2.	Results will be displayed in the Selected Data tab-panel
ii.	Add Column
1.	Download the template and add data values to that template and upload it click on the Combine Column button
2.	Results will be displayed in the Selected Data tab-panel
iii.	Add Match Column
1.	Choose Math Function
a.	Arithmetic Mean
i.	Will calculate AM of two or more columns and creates the new column 
b.	Geometric Mean
i.	Will calculate GM of two or more columns and creates the new column
c.	Addition
i.	Will calculate Addition of two or more columns and creates the new column
2.	New column name should be mentioned in the input box by the user. 
3.	Manipulated data will be displayed in the Selected Data tab-panel
