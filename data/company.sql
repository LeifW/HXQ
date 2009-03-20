create table employee
	( fname		varchar(15)	not null,
	  minit		char,
	  lname		varchar(15)	not null,
	  ssn		char(9)		primary key not null,
	  dno		int		references department(dnumber),
	  bdate		date,
	  address	varchar(30),
	  sex		char,
	  salary	decimal(10,2),
	  superssn	char(9)		references employee(ssn) );


create table department
	( dname		varchar(15)	unique not null,
	  dnumber	int		primary key not null,
	  mgrssn	char(9)		references employee(ssn),
	  mgrstartdate	date		);


create table dept_locations
	( dnumber	int		not null references department(dnumber),
	  dlocation	varchar(15)	not null,
	primary key (dnumber,dlocation) );


create table project
	( pname		varchar(15)	unique not null,
	  pnumber	int		primary key not null,
	  plocation	varchar(15),
	  dnum		int		references department(dnumber) );


create table works_on
	( essn		char(9)		not null references employee(ssn),
	  pno		int		not null references project(pnumber),
	  hours		decimal(3,1),
	primary key (essn,pno)		);


create table dependent
	( essn		char(9)		not null references employee(ssn),
	  dependent_name varchar(15)	not null,
	  sex		char,
	  bdate		date,
	  relationship	varchar(8),
	primary key (essn,dependent_name) );



insert into department
	values ( 'Research', 5, '22-MAY-78', 333445555 );

insert into department
	values ( 'Administration', 4, '01-JAN-85', 987654321 );

insert into department
	values ( 'Headquarters', 1, '19-JUN-71', 888665555 );


insert into employee
	values ( 'James', 'E', 'Borg', 888665555, 1, '10-NOV-27', '450 Stone, Houston, TX', 'M', 55000, null );

insert into employee
	values	( 'Franklin', 'T', 'Wong', 333445555, 5, '18-DEC-45', '638 Voss, Houston, TX', 'M', 40000, 888665555);

insert into employee
	values	( 'John', 'B', 'Smith', 123456789, 5, '09-JAN-55', '731 Fondren, Houston, TX', 'M', 30000, 333445555 );

insert into employee
	values	( 'Jennifer', 'S', 'Wallace', 987654321, 4, '20-JUN-31', '291 Berry, Bellair, TX', 'F', 43000, 888665555 );

insert into employee
	values	( 'Alicia', 'J', 'Zelaya', 999887777, 4, '19-JUL-58', '3321 Castle, Spring, TX', 'F', 25000, 987654321 );

insert into employee
	values	( 'Ramesh', 'K', 'Narayan', 666884444, 5, '15-SEP-52', '975 Fire Oak, Humble, TX', 'M', 38000, 333445555 );

insert into employee
	values	( 'Joyce', 'A', 'English', 453453453, 5, '31-JUL-62', '5631 Rice, Houston, TX', 'F', 25000, 333445555 );

insert into employee
	values	( 'Ahmad', 'V', 'Jabbar', 987987987, 1, '29-MAR-59', '980 Dallas, Houston, TX', 'M', 25000, 987654321 );


insert into dept_locations
	values ( 1, 'Houston' );

insert into dept_locations
	values ( 4, 'Stafford' );

insert into dept_locations
	values ( 5, 'Bellaire' );

insert into dept_locations
	values ( 5, 'Sugarland' );

insert into dept_locations
	values ( 5, 'Houston' );


insert into project
	values ( 'ProductX', 1, 'Bellaire', 5 );

insert into project
	values ( 'ProductY', 2, 'Sugarland', 5 );

insert into project
	values ( 'ProductZ', 3, 'Houston', 5 );

insert into project
	values ( 'Computerization', 10, 'Stafford', 4 );

insert into project
	values ( 'Reorganization', 20, 'Houston', 1 );

insert into project
	values ( 'Newbenefits', 30, 'Stafford', 4 );


insert into works_on
	values ( 123456789, 1, 32.5 );

insert into works_on
	values ( 123456789, 2, 7.5 );

insert into works_on
	values ( 666884444, 3, 40.0 );

insert into works_on
	values ( 453453453, 1, 20.0 );

insert into works_on
	values ( 453453453, 2, 20.0 );

insert into works_on
	values ( 333445555, 2, 10.0 );

insert into works_on
	values ( 333445555, 3, 10.0 );

insert into works_on
	values ( 333445555, 10, 10.0 );

insert into works_on
	values ( 333445555, 20, 10.0 );

insert into works_on
	values ( 999887777, 30, 30.0 );

insert into works_on
	values ( 999887777, 10, 10.0 );

insert into works_on
	values ( 987987987, 10, 35.0 );

insert into works_on
	values ( 987987987, 30, 5.0 );

insert into works_on
	values ( 987654321, 30, 20.0 );

insert into works_on
	values ( 987654321, 20, 15.0 );

insert into works_on
	values ( 888665555, 20, null );


insert into dependent
	values ( 333445555, 'Alice', 'F', '05-APR-76', 'DAUGHTER' );

insert into dependent
	values ( 333445555, 'Theodore', 'M', '25-OCT-73', 'SON' );

insert into dependent
	values ( 333445555, 'Joy', 'F', '03-MAR-48', 'SPOUSE' );

insert into dependent
	values ( 987654321, 'Abner', 'M', '29-FEB-32', 'SPOUSE' );

insert into dependent
	values ( 123456789, 'Michael', 'M', '01-JAN-78', 'SON' );

insert into dependent
	values ( 123456789, 'Alice', 'F', '31-DEC-78', 'DAUGHTER' );

insert into dependent
	values ( 123456789, 'Elizabeth', 'F', '05-MAY-57', 'SPOUSE' );
