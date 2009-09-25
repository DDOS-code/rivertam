----------------------------------------------------------------------------
-- Import the clans to your local database with:
-- psql -d DATABASENAME -f clans_dump.sql
-- Dumped with: pg_dump rivertam -t clans -x -O > clans_dump.sql
----------------------------------------------------------------------------

-- PostgreSQL database dump
--
--
-- Name: clans; Type: TABLE; Schema: public; Owner: -; Tablespace: 
--

CREATE TABLE clans (
    id integer NOT NULL,
    tag text NOT NULL,
    name text NOT NULL,
    irc text DEFAULT ''::text,
    homepage text DEFAULT ''::text
);


--
-- Name: clans_id_seq; Type: SEQUENCE; Schema: public; Owner: -
--

CREATE SEQUENCE clans_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MAXVALUE
    NO MINVALUE
    CACHE 1;


--
-- Name: clans_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: -
--

ALTER SEQUENCE clans_id_seq OWNED BY clans.id;


--
-- Name: clans_id_seq; Type: SEQUENCE SET; Schema: public; Owner: -
--

SELECT pg_catalog.setval('clans_id_seq', 131, true);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: -
--

ALTER TABLE clans ALTER COLUMN id SET DEFAULT nextval('clans_id_seq'::regclass);


--
-- Data for Name: clans; Type: TABLE DATA; Schema: public; Owner: -
--

COPY clans (id, tag, name, irc, homepage) FROM stdin;
69	.de	Tremmany		
126	Fear|	Fear		
8	GEZ|	GEZ	#GEZ @ quakenet	http://trem-servers.com
127	Waterguns|	Waterguns		
23	[-&-]	Unity		
46	[mDc]	Mass Driver Club		
12	(eVo)	eVolution	##(eVo) @ freenode	http://serdinho.se.funpic.de
35	[HUN]	Hungary		
128	FoT|	Freaks on Tour		http://freaksontour.forumotion.com
129	{GoW*	Gods of War		http://gowclan.phorum.cz
24	virus|	Virus		
65	DSPro|	Dretch*Storm Pro		
66	|KoR|	Knights of Reason		http://knightsofreason.org
54	{Hyb}	Hybrid		
130	D)	Darkness		
73	|POC|	Prophets of Carnage	##poc-clan @ freenode	http://poc-clan.eu
77	Ru!	Russians !ncoming	##Ru! @ freenode	http://rui.clan.su
78	[L]	Lubiteli		http://tremworld.com
80	IIIIII	Just4Fun		http://j4fun.clan.su
82	[OPP]	Oppressed		http://clan.oppressed.net
85	Derelict	Derelict	#derelict @ freenode	http://derelict.freeforums.org
86	LP'	Lucifer Profanation		http://clan-lp.eu
88	Welcomed	Welcomed		http://welcomeddread.proboards.com
89	{KOC}	Kocour		http://volny.cz/koc-team
91	{wwF*	World Wide Froggies	#wwF @ freenode	http://wwf-team.fr
92	CU|	Campers United		http://cu-clan.net
94	V I K I N G S |	Vikings	##trem-vikings @ freenode	
95	[!!!]	Triplex Alliance	##triplex	http://www.triplexalliance.org
96	(CY)	Yggdrasil's Brotherhood	#CY @ freenode	
97	vD|	Void		vdclan.forumotion.net
98	{&}	The Sea Men Syndicate		http://seamen.sikiri.com
99	{NoS}	Nation of Stupidity	##acnos @ freenode	http://acnos.imn2rc.com/forum
102	|AoD|	Arsenal of Democracy		http://arsenalofdemocracy.co.cc
103	[XD]	eXtreme Duty		
104	}MG{	Mercenaries Guild	#mercenariesguild @ freenode	http://mercenariesguild.net
71	(><)	Xenocide		http://interpsy.com/x
105	[CoW]	Core of War		http://cow.kilo-moto.com
106	One|	One Clan	#oneclan @ freenode	http://oneclan.bplaced.net
107	=V=	Venergetic		
109	(kiwi.	kiwi Clan		
110	[Z|R]	Zero Risk		
111	[F]lame	[F]lame		http://flame.dyndns.org
112	=Pk|:	Player Killers		http://clanpk.org
114	[EBSF]	Elite Blaster Strike Force		
116	[v4]	vengeance4		http://tremulousclan.smfnew.com
117	[vR]	Revolt		
118	(Nova|	NovaTiTude	##(nova) @ freenode	http://novaclan.siteboard.de
119	.^	Alcoholics Anonymous		http://aaclan.mywebcommunity.org/
120	+Zilla	Zilla		
122	.bH	bH clan		http://bhclan.forumotion.com
1	.ddos	Distributed Denial Of Skill	##ddos @ freenode	http://ddos-tremulous.eu
2	mYm	Meet Your Makers		
70	/>	<TAG/>	##TAG	http://tag.develz.org
87	r3v:	r3volution	#r3v @ freenode	http://r3vclan.net
53	.!s	Inflicted Sanction		
30	{iTa}	Italian Tremulous Alliance	##tremulous.ita	http://www.tremulous.it
22	|FSN	FuSioN		
100	{A}	Ancients		http://ancients.clangrid.com/
19	|sCc|	Spanish Community Clan		http://scc.dreamhosters.com
90	{tHc}	The Hell With Campers		http://thcteam.org
121	lead|	Leaders		
124	(Inc.	Incoming		
18	[SWISS]	SWISS		http://swiss.dekebo.de
5	-CZ-	CamperZ		http://camperz.cz
4	YK@	Yankee		http://yk.glt.pl
6	{chess}	Chess		
7	[HSR]	Hussars		
113	|SoH|	Saviors of Humor		http://soh.benburhans.com/forum
9	($)	Shasta		
14	{aMa}	Amazons	#amazons @ quakenet	
62	[NULL]	NULL		
60	[<3]	Less than Three		
37	(J)FT	(J)FT		
75	@tm	@tm	@tm @ freenode	http://atm.yah.fi
13	|MoT|	MoT		
\.


--
-- Name: clans_pkey; Type: CONSTRAINT; Schema: public; Owner: -; Tablespace: 
--

ALTER TABLE ONLY clans
    ADD CONSTRAINT clans_pkey PRIMARY KEY (id);


--
-- Name: clans_nocase; Type: INDEX; Schema: public; Owner: -; Tablespace: 
--

CREATE UNIQUE INDEX clans_nocase ON clans USING btree (lower(tag));


--
-- PostgreSQL database dump complete
--

