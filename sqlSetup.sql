create table sketch.SavedPictures 
(
	[Id] INT NOT NULL PRIMARY KEY,
	[Owner] NVARCHAR(100) NULL,
	[JSON] NVARCHAR(max) not null
)
go
Create index IX_ByNameAndId on sketch.SavedPictures(Owner,Id)