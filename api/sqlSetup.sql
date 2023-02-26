-- run this via some manual method

-- drop table sketch.SavedPicture
create schema sketch
go
create table sketch.SavedPicture
(
	[Id] INT IDENTITY NOT NULL PRIMARY KEY,
	[Tag] NVARCHAR(100) NOT NULL,
	[Owner] NVARCHAR(100) NULL,
	[JSON] NVARCHAR(max) not null,
	constraint Unique_IX_ByNameAndTag unique(Owner,Tag)
)

GO
create or alter procedure sketch.sp_savePicture @tag nvarchar(100), @owner nvarchar(100), @json nvarchar(max)
as
merge sketch.savedPicture as target
using (select @tag as tag, nullif(trim(@owner), '') as owner, @json as json) as src
	on (target.tag is not distinct from src.tag) and (src.owner is not distinct from target.owner)
when matched then
	update set target.json = src.json
when not matched then insert(tag, owner, json) values(src.tag, src.owner, src.json);
