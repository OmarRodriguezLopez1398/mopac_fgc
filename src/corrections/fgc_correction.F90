!                            PM6-FGC Correction  
!    Pérez-Tabero, S.; Fernández, B.; Cabaleiro-Lago, E. M.; Martínez-Núñez, E.;
!    Vázquez, S. A. Journal of Chemical Theory and Computation 2021, 17, 5556–5567.
!
!    Ríos-García, M.; Fernández, B.; Rodríguez-Otero, J.; Cabaleiro-Lago, E. M.;
!                    Vázquez, S. A. Molecules 2022, 27, 1678.
!            
!    Cabaleiro-Lago, E. M.; Fernández, B.; Rodríguez-Fernández, R.; Rodríguez-Otero, J.;
!                   Vázquez, S. A. J. Chem. Phys. 158, 124105 (2023).
!
! In this file two section can be distingish, the first one is a
! module which contains the parameters for the PM6-FGC correction, 
! while the second is a subroutine which calculates the 
! correction itself.
!
! Module made by two functions, the first with the character 
! data-type, while the second contain the floating point 
! parameters, both in matrix form. The function gives 
! the element of that matrix which is desired in the 
! correction calculation
!
module fgc_correction
implicit none
public :: fgc_parameters, fgc_symbols
contains

function fgc_symbols(i,j) result(symbol)

! Function with the character type part of the parameters
! These are the keywords for the parameters

implicit none

! Only two dummy variables

integer, intent(in)               :: i, j

! Variables of the code

character(len=10)                 :: symbol 
! ------------------------------Cambiar aqui el n=165
integer, parameter                :: n=105, m=2 
character(len=10), dimension(n,m) :: T

! Keywords contained in a matrix

T(1,:)=(/"        CA","        CA"/)
T(2,:)=(/"        CA","       CAD"/)
T(3,:)=(/"        CA","       NAD"/)
T(4,:)=(/"        CA","       OAD"/)
T(5,:)=(/"        CA","      HNAD"/)
T(6,:)=(/"        CA","       HCA"/)
T(7,:)=(/"        CA","       CCA"/)
T(8,:)=(/"        CA","      OCA2"/)
T(9,:)=(/"        CA","      OCA1"/)
T(10,:)=(/"        CA","      HOCA"/)
T(11,:)=(/"        CA","       NAN"/)
T(12,:)=(/"        CA","      HNAN"/)
T(13,:)=(/"        CA","       CAN"/)
T(14,:)=(/"        CA","      HCAN"/)
T(15,:)=(/"       CAD","       CAD"/)
T(16,:)=(/"       CAD","       NAD"/)
T(17,:)=(/"       CAD","       OAD"/)
T(18,:)=(/"       CAD","      HNAD"/)
T(19,:)=(/"       CAD","       HCA"/)
T(20,:)=(/"       CAD","       CCA"/)
T(21,:)=(/"       CAD","      OCA2"/)
T(22,:)=(/"       CAD","      OCA1"/)
T(23,:)=(/"       CAD","      HOCA"/)
T(24,:)=(/"       CAD","       NAN"/)
T(25,:)=(/"       CAD","      HNAN"/)
T(26,:)=(/"       CAD","       CAN"/)
T(27,:)=(/"       CAD","      HCAN"/)
T(28,:)=(/"       NAD","       NAD"/)
T(29,:)=(/"       NAD","       OAD"/)
T(30,:)=(/"      HNAD","       NAD"/)
T(31,:)=(/"       HCA","       NAD"/)
T(32,:)=(/"       CCA","       NAD"/)
T(33,:)=(/"       NAD","      OCA2"/)
T(34,:)=(/"       NAD","      OCA1"/)
T(35,:)=(/"      HOCA","       NAD"/)
T(36,:)=(/"       NAD","       NAN"/)
T(37,:)=(/"      HNAN","       NAD"/)
T(38,:)=(/"       CAN","       NAD"/)
T(39,:)=(/"      HCAN","       NAD"/)
T(40,:)=(/"       OAD","       OAD"/)
T(41,:)=(/"      HNAD","       OAD"/)
T(42,:)=(/"       HCA","       OAD"/)
T(43,:)=(/"       CCA","       OAD"/)
T(44,:)=(/"       OAD","      OCA2"/)
T(45,:)=(/"       OAD","      OCA1"/)
T(46,:)=(/"      HOCA","       OAD"/)
T(47,:)=(/"       NAN","       OAD"/)
T(48,:)=(/"      HNAN","       OAD"/)
T(49,:)=(/"       CAN","       OAD"/)
T(50,:)=(/"      HCAN","       OAD"/)
T(51,:)=(/"      HNAD","      HNAD"/)
T(52,:)=(/"       HCA","      HNAD"/)
T(53,:)=(/"       CCA","      HNAD"/)
T(54,:)=(/"      HNAD","      OCA2"/)
T(55,:)=(/"      HNAD","      OCA1"/)
T(56,:)=(/"      HNAD","      HOCA"/)
T(57,:)=(/"      HNAD","       NAN"/)
T(58,:)=(/"      HNAD","      HNAN"/)
T(59,:)=(/"       CAN","      HNAD"/)
T(60,:)=(/"      HCAN","      HNAD"/)
T(61,:)=(/"       HCA","       HCA"/)
T(62,:)=(/"       CCA","       HCA"/)
T(63,:)=(/"       HCA","      OCA2"/)
T(64,:)=(/"       HCA","      OCA1"/)
T(65,:)=(/"       HCA","      HOCA"/)
T(66,:)=(/"       HCA","       NAN"/)
T(67,:)=(/"       HCA","      HNAN"/)
T(68,:)=(/"       CAN","       HCA"/)
T(69,:)=(/"       HCA","      HCAN"/)
T(70,:)=(/"       CCA","       CCA"/)
T(71,:)=(/"       CCA","      OCA2"/)
T(72,:)=(/"       CCA","      OCA1"/)
T(73,:)=(/"       CCA","      HOCA"/)
T(74,:)=(/"       CCA","       NAN"/)
T(75,:)=(/"       CCA","      HNAN"/)
T(76,:)=(/"       CAN","       CCA"/)
T(77,:)=(/"       CCA","      HCAN"/)
T(78,:)=(/"      OCA2","      OCA2"/)
T(79,:)=(/"      OCA1","      OCA2"/)
T(80,:)=(/"      HOCA","      OCA2"/)
T(81,:)=(/"       NAN","      OCA2"/)
T(82,:)=(/"      HNAN","      OCA2"/)
T(83,:)=(/"       CAN","      OCA2"/)
T(84,:)=(/"      HCAN","      OCA2"/)
T(85,:)=(/"      OCA1","      OCA1"/)
T(86,:)=(/"      HOCA","      OCA1"/)
T(87,:)=(/"       NAN","      OCA1"/)
T(88,:)=(/"      HNAN","      OCA1"/)
T(89,:)=(/"       CAN","      OCA1"/)
T(90,:)=(/"      HCAN","      OCA1"/)
T(91,:)=(/"      HOCA","      HOCA"/)
T(92,:)=(/"      HOCA","       NAN"/)
T(93,:)=(/"      HNAN","      HOCA"/)
T(94,:)=(/"       CAN","      HOCA"/)
T(95,:)=(/"      HCAN","      HOCA"/)
T(96,:)=(/"       NAN","       NAN"/)
T(97,:)=(/"      HNAN","       NAN"/)
T(98,:)=(/"       CAN","       NAN"/)
T(99,:)=(/"      HCAN","       NAN"/)
T(100,:)=(/"      HNAN","      HNAN"/)
T(101,:)=(/"       CAN","      HNAN"/)
T(102,:)=(/"      HCAN","      HNAN"/)
T(103,:)=(/"       CAN","       CAN"/)
T(104,:)=(/"       CAN","      HCAN"/)
T(105,:)=(/"      HCAN","      HCAN"/)



! Variable which returns the desired pairwise keyword 

symbol=T(i,j)

return

end function fgc_symbols

function fgc_parameters(i,j) result(value)

! Function with the character type part of the parameters
! These are the keywords for the parameters

implicit none

! Only two dummy variables as the previous function

integer, intent(in)          :: i, j

! Variables of the code

real(kind=8)                 :: value
! -----------------cambiar aqui n=165 y m seria 6 porque habra otro parametro
integer, parameter           :: n=105, m=6
real(kind=8), dimension(n,m) :: C

! Numerical parameters contained in a matrix

C(1,:) = (/54744.815249982435,2.977238563065,-4092.765190005646,5.020266181668,2.695604143708,7.286036849188/)
C(2,:) = (/42667.05971303708,3.286245687825,377.674211218267,8.061983558544,2.033182584328,26.961702878425/)
C(3,:) = (/79643.38121173024,3.644390960372,679.839949732246,7.501064774763,1.794851728999,2.189368188642/)
C(4,:) = (/114557.0434436713,3.365890407445,-258.107743707135,6.865718719775,1.82121071789,7.018360507428/)
C(5,:) = (/-32618.25816173911,6.121745291158,-1303.204443217822,7.073345374076,1.228504889499,3.880193110874/)
C(6,:) = (/-49108.38248388185,4.73996945345,-136.370070573784,5.425312866327,1.200852149436,3.568041428836/)
C(7,:) = (/50269.47503948895,4.307510170788,-147.967244953577,6.897019784385,2.576772944266,7.715700805138/)
C(8,:) = (/91700.51282108117,3.678981055286,588.78687886898,6.030230656906,1.647178801016,2.544892250252/)
C(9,:) = (/108078.694017041,3.494665508557,-2270.423373974923,6.296553877373,1.664813096332,3.091345954267/)
C(10,:) = (/4619.352302133692,4.325337810861,-1317.687916246305,7.023570364884,1.160666861078,3.634846183236/)
C(11,:) = (/37932.98708059857,2.950741853323,-425.788415173556,7.647119289927,1.90664930838,2.277215921978/)
C(12,:) = (/-12699.539095511394,3.253478148463,377.359025517518,5.430732142903,1.201258586081,3.070005243617/)
C(13,:) = (/44691.74626378296,3.137856127462,-1773.455578143094,5.06816746309,2.699795047535,11.49785804542/)
C(14,:) = (/-30270.265935358824,4.303563980546,897.937748237493,7.813267944909,1.641024080354,6.469479473048/)
C(15,:) = (/-38553.93882186441,2.610003245343,-2764.595735894279,5.760697024089,2.451382545557,3.882560129037/)
C(16,:) = (/-136482.8904221638,4.040604666968,-821.252372236126,7.981543392381,1.942638006887,20.059286015269/)
C(17,:) = (/-25509.477550404765,4.245100190228,-575.779042429114,6.72868063699,2.151687830308,1.984232392069/)
C(18,:) = (/4714.146259442027,2.476139453234,-3173.80129922676,7.106719024331,1.350983455189,5.67167222922/)
C(19,:) = (/-70284.50622376087,5.103169704801,-137.458781389722,5.788554784253,0.941586417175,6.194460295539/)
C(20,:) = (/77160.33976631792,3.272977117818,-870.427641589159,5.343564138201,2.48901492556,14.179454179417/)
C(21,:) = (/-53270.09283148585,4.043565161899,-299.638910122294,7.209831717129,2.688077847911,17.67625862019/)
C(22,:) = (/-32884.18564589313,2.92990452979,-2445.049794166152,5.745213746293,2.331313072109,4.74043868252/)
C(23,:) = (/54440.87815646867,5.558713614772,662.20641918951,4.215870549782,2.530993586039,23.523492032625/)
C(24,:) = (/-115166.36136327205,4.35821038025,1071.811919435484,6.860282899905,2.544595616102,14.567959679851/)
C(25,:) = (/-37066.436310614015,5.883693026971,-1146.14469633266,8.970220104229,1.248455399292,5.641260934702/)
C(26,:) = (/70045.39049059416,3.090487303685,-2394.208157972239,4.964899030616,2.386883999631,19.647291792934/)
C(27,:) = (/2278.494272024753,2.788871935963,-1429.817555869873,6.004577276916,1.176018635166,2.034196134784/)
C(28,:) = (/90754.68306662426,3.057295216481,274.951510033198,5.755629607893,1.772085164599,4.962153804388/)
C(29,:) = (/125774.89371543255,3.412730798146,565.524943941045,6.87847955619,1.882103848618,2.155186098785/)
C(30,:) = (/7305.998817298943,3.924742171644,-933.768618860165,4.640483594486,1.11285955873,4.708902705201/)
C(31,:) = (/-5710.626966444077,6.318480350484,-115.087109096293,6.917467268835,1.766575999747,1.57745130039/)
C(32,:) = (/-104272.68235942308,3.540548388824,-442.908756217786,8.296445771248,2.229414567201,3.918284333914/)
C(33,:) = (/108243.20372485096,3.319741762334,-270.225992000205,6.865221330512,1.490624556371,2.203585522966/)
C(34,:) = (/78018.29583785136,3.154892005772,703.521108451232,6.501448622837,1.604801175969,2.896797280277/)
C(35,:) = (/8338.034148349609,5.607278502901,-833.157783618194,4.773632885816,0.991497968509,10.58960315675/)
C(36,:) = (/79854.51248616033,3.096255593718,30.350877997618,4.331338809149,1.813809612731,2.334239218778/)
C(37,:) = (/7775.876020818245,4.728316484143,-1607.130932691343,6.294121916028,1.315941229326,2.945800527395/)
C(38,:) = (/-10841.003213542805,4.552077021706,1103.317470327342,6.5389234337,1.970833372741,13.730459310879/)
C(39,:) = (/-82460.19720912236,5.250625391818,869.220883087298,5.812979467099,1.520393260983,7.916324088339/)
C(40,:) = (/125162.26472138056,3.806510148453,1064.487428110783,6.324583760813,1.968130267059,4.872657969964/)
C(41,:) = (/11835.77551405132,3.802445856549,-613.394176338575,5.239144369117,1.011104500989,4.194989932322/)
C(42,:) = (/14682.152291378437,3.703726912067,-1565.312485019696,10.100788884449,1.632094151755,5.412949025796/)
C(43,:) = (/39414.37500205432,5.3111505891,-2350.519377971966,4.951663007182,2.052974884356,3.746737863123/)
C(44,:) = (/67798.19914065761,3.20452395665,-53.548519902737,5.449390936503,1.743139806674,2.402090283556/)
C(45,:) = (/46767.583210714,3.89535301146,1115.887978002004,4.520754104753,1.759062869964,1.831436657785/)
C(46,:) = (/46307.31361388486,5.115413405484,-480.006104824313,5.032941250976,0.805491789168,9.087290359179/)
C(47,:) = (/88496.5894710291,3.228619809546,291.189498507388,7.610503991153,1.964344672571,2.772238729697/)
C(48,:) = (/11868.592480504918,5.981732309437,-648.210412940803,6.951807864648,1.246330078969,3.150384198869/)
C(49,:) = (/57515.50226369081,3.326166853448,880.535267563003,5.148833859997,1.301968619831,10.480342954756/)
C(50,:) = (/3406.981067806709,6.154222835779,665.373986296374,7.974455514962,1.273307244516,4.468752287171/)
C(51,:) = (/-3121.516234002876,4.597758912726,199.943135774291,3.88982038502,1.12618702219,1.340501256074/)
C(52,:) = (/1815.397624095266,3.355816857935,0.0,0.0,0.861894317723,9.573785670815/)
C(53,:) = (/8976.160189308333,3.147653123431,299.326108857268,9.176629157355,2.013556919546,4.837524853373/)
C(54,:) = (/19694.397570158297,3.780037271127,-805.996917335993,5.233856834291,1.062642347902,2.05262502756/)
C(55,:) = (/9972.737319615131,4.183818546113,-393.784645636807,5.191236784768,0.962802845873,2.264038203591/)
C(56,:) = (/6828.449436445986,3.431752724153,838.168532334891,7.768260855367,1.224234011091,7.6332581296/)
C(57,:) = (/96411.28753128048,4.666390553774,-1579.963037379127,5.842380447465,1.218841006603,2.305851932349/)
C(58,:) = (/12040.708250299613,3.774242620013,-389.801975923022,8.325407957935,1.102190544518,2.659177494272/)
C(59,:) = (/40440.46124721035,3.939895142917,-2767.340312671666,6.23247225399,1.437071405895,3.145835073325/)
C(60,:) = (/-73766.95849694494,5.964510645864,553.193106158066,7.719193240662,1.176650870273,1.974222856151/)
C(61,:) = (/-14164.81690965208,4.736217097616,401.94436253831,6.233001753687,1.021728864483,6.357841478167/)
C(62,:) = (/-9202.964527451266,4.645505194856,-870.640812539201,7.571878077585,1.088246328899,3.062074369059/)
C(63,:) = (/7247.964603698509,2.803643779793,-711.248895431123,5.54504493851,0.963670099232,3.333519388121/)
C(64,:) = (/6662.757148362528,4.726930996932,1100.363456139573,6.733810377602,1.439819118231,6.579749402983/)
C(65,:) = (/14120.418778488265,4.945713258895,0.0,0.0,0.929958175328,14.864053729466/)
C(66,:) = (/41374.274040497876,4.968613590618,-501.776318728697,8.06208383365,1.647426459061,2.817580449025/)
C(67,:) = (/58698.526979478076,5.617235931678,-137.170817703829,7.789004453686,0.910961273088,12.916136398077/)
C(68,:) = (/-38911.09719300269,4.22555785181,285.36288897672,7.689493722234,1.260923015523,15.538794493808/)
C(69,:) = (/7539.737633787478,5.338997372232,164.99574911645,6.768971340295,1.061139995732,2.004487869828/)
C(70,:) = (/84793.17115279207,4.961483580995,-2855.334862541076,4.962916214861,1.590603599133,11.832303723515/)
C(71,:) = (/101279.607592857,5.371246814213,-1916.977146405864,6.21442375648,1.480824262765,12.237214541872/)
C(72,:) = (/17073.3838225363,3.975557777119,-595.312665187653,10.474385164033,1.79642144912,18.833477492116/)
C(73,:) = (/-128171.25947217195,4.558581116018,279.303912653205,3.237368085643,1.326006074523,12.425724610167/)
C(74,:) = (/-3848.168101710116,4.413402433864,736.278159814572,3.850363778983,2.761773001023,15.329986732632/)
C(75,:) = (/-37749.68017331004,4.945526237073,-418.052020989058,7.034106772947,1.123553262035,4.610465320247/)
C(76,:) = (/103636.75275299874,3.992111962302,-1782.938959435072,4.658147134688,2.031045605223,11.054281735545/)
C(77,:) = (/-134128.5653816432,5.095947018395,294.819748927917,6.1396733519,1.145680713165,2.823677288252/)
C(78,:) = (/281062.84877002746,3.594912495205,-1567.815478985447,5.618171021096,1.714520560282,11.521866243782/)
C(79,:) = (/221145.5862455137,3.426912653575,-1145.043939478145,5.937909279772,1.693474835967,15.153751016111/)
C(80,:) = (/2053.160281721024,3.8998424206,-413.211729301839,4.978988617882,1.019557547213,4.861761935157/)
C(81,:) = (/77476.9205713548,3.336998590103,785.015186247817,6.396550329661,1.583137132312,1.506585558163/)
C(82,:) = (/74912.51030309951,6.019772144994,-478.419398570462,7.441048089962,1.096180496363,3.241441582506/)
C(83,:) = (/96848.29808255952,3.354060934642,-424.179325294651,5.238960715247,1.823252505517,1.811060705323/)
C(84,:) = (/23442.35166819612,3.923431797964,-248.358725909394,8.197815210239,1.358544506482,8.417624355874/)
C(85,:) = (/151556.94737109408,3.731254186865,-501.174158337435,9.433782511448,1.602396222769,3.154558402967/)
C(86,:) = (/-11961.027403281247,6.734872801558,-239.72400532515,3.275035946628,0.89949039652,5.502680757443/)
C(87,:) = (/83894.65804689356,3.132588184401,-1538.00187814403,5.284727442267,1.705665237163,10.760950798525/)
C(88,:) = (/-6199.983890532883,4.72505223991,-1278.491484529705,8.081129999529,1.272833825246,12.411196294908/)
C(89,:) = (/62476.771849327306,3.039128800325,356.744608543136,8.816432259972,1.756133785287,5.937066263069/)
C(90,:) = (/-57878.52511684889,7.1873105717,0.0,0.0,1.705346942881,6.123398481389/)
C(91,:) = (/2957.863761282137,3.198727841637,0.0,0.0,0.841667633583,12.295614560081/)
C(92,:) = (/93937.49693934222,4.57656101897,-1244.701574525243,5.021054245944,0.925240451981,2.861656966275/)
C(93,:) = (/-45766.117577267825,4.834487676753,729.74310915931,6.324484578714,1.10435656131,3.159092273935/)
C(94,:) = (/34631.34124109994,5.759627068803,-1386.434028306543,6.38753320495,1.071346869338,4.556921490181/)
C(95,:) = (/-76292.62286294601,5.945983138258,1310.700822635842,8.923113473858,1.105321357256,13.600290189695/)
C(96,:) = (/85634.31878039964,3.095927841819,-1809.186349075436,5.990130747046,2.382723094464,3.846101663376/)
C(97,:) = (/64158.32310136428,5.02556165918,-656.397480193426,5.950056216787,1.016782081494,21.551728986441/)
C(98,:) = (/19562.282949133816,2.930313284282,-1063.159616385132,5.15683153549,2.168409850456,6.535129531498/)
C(99,:) = (/-80740.65803116612,4.768940980082,2794.060927999773,6.720636536182,1.493691099292,7.643851185478/)
C(100,:) = (/13469.696557269555,3.523342104393,-451.735532336642,5.376807899525,0.747010870087,12.885354650485/)
C(101,:) = (/-19728.539155810784,3.561755751799,334.008417102419,7.037008702334,1.175201965687,6.638293782708/)
C(102,:) = (/14400.117050433288,4.283959995841,-162.335052679888,6.248439538866,1.109341854091,3.752134272678/)
C(103,:) = (/66853.0047302846,3.344487310811,-1733.163169372891,5.540419098133,2.223605153745,16.802651335646/)
C(104,:) = (/85901.42601404831,6.259744028415,-1142.762162884555,6.528236488637,1.41611269034,11.997701479651/)
C(105,:) = (/-132021.07464137088,5.574358475698,1038.749369406696,7.616964004118,0.961632496749,11.371205286681/)



! Variable which returns the desired pairwise keyword 

value=C(i,j)

return

end function fgc_parameters

end module 

! External subroutine to calculate the correction itself
! It uses some data from the system collected from MOPAC 
! and the parameters from the module above
! Also, it needs to be provided a setup file with each pair of 
! atoms of the system and their atoms types for when atoms are 4 or 
! more bonds far from eachother

subroutine fgc(numat,el,x,y,z,fgc_energy,driv) 

! Uses the functions with the parameters

    use fgc_correction, only : fgc_parameters, fgc_symbols
    implicit none

! Dummy variables collected from MOPAC main program
! The ones with intent(out) are optional which allow to differienciate between 
! energy and derivatives calculation

    integer, intent(in)                     :: numat 
    character(len=2), intent(in)            :: el(:)
    double precision, intent(in)            :: x(:), y(:), z(:)
    double precision, intent(out), optional :: fgc_energy, driv(3,numat) 

! Local Variables

    integer                                 :: dd, ddd, stat, i, j, l, a, b, lo
    !-------------------------------Cambiar aqui n y m
    integer, parameter                      :: n=105, m=8
    integer, allocatable                    :: ind(:), jnd(:)
    double precision, allocatable           :: d(:)
    ! --------------------------------------------agregar aqui el paramF
    double precision                        :: buf, paramA, paramB, paramC, paramD, paramE, paramF, &
                                            ppair, fswitch, energy, r, value, dri, dVr, drx, &
					    dry, drz
    character(len=10)                       :: p1, p2, symbol
    character(len=10), allocatable          :: pp1(:), pp2(:)

! If loop to differiantate between energy (this first argument) or derivatives(bellow)

    if (present(fgc_energy)) then

! Opens the setup file with atom types of all possible pairs of atoms
! and then the programs looks for the total length of the file

       open(90, file="setup.txt", status="OLD")
       dd=0
       do
          read(90,*,iostat=stat) buf
          if (stat/=0) exit
          dd=dd+1
       enddo

! Allocate some arrays to contain data from setup file 

       allocate(d(dd), pp1(dd), pp2(dd), ind(dd), jnd(dd))

! Rewind the file and collect all important data from setup

       rewind(90)

       30 FORMAT (1X, I4, 1X, I4, 1X, A4, 1X, A10, 1X, A10)
       dri=0
       do j=1, dd
          read(90,30)ind(j),jnd(j),d(j),pp1(j),pp2(j)
       enddo
       rewind(90)

! Initiallize some important variables

       fgc_energy=0.d0
       energy=0.d0

! Loop to go through all pairs of atoms described in the setup file
! and calculate the distance between atoms of each pair

       do j=1, dd
          r=sqrt((x(ind(j))-x(jnd(j)))**2+(y(ind(j))-y(jnd(j)))**2+ &
	  (z(ind(j))-z(jnd(j)))**2)
          ppair=0

! Another loop, this one to try to find the same keywords for the available 
! parameters for the correction with if loop to find which pairs match
! Also the loop if include a second part to if the order is different
! If they match, the energy for that pair is calculated and added to the 
! one from previous matched pairs

             do i=1, n 
                p1=fgc_symbols(i,1)
                p2=fgc_symbols(i,2)
                if (p1==pp1(j) .and. p2==pp2(j)) then
                   paramA=fgc_parameters(i,1)
                   paramB=fgc_parameters(i,2)
                   paramC=fgc_parameters(i,3)
                   paramD=fgc_parameters(i,4)
                   paramE=fgc_parameters(i,5)
                   paramF=fgc_parameters(i,6)
                   fswitch = 1.0d0 / (1.0d0 + paramF * (paramE / r)**12)
                   ppair=fswitch*(paramA*exp(-paramB*r)+paramC/(r**paramD))
                   energy=energy+ppair
                elseif (p2==pp1(j) .and. p1==pp2(j)) then
                   paramA=fgc_parameters(i,1)
                   paramB=fgc_parameters(i,2)
                   paramC=fgc_parameters(i,3)
                   paramD=fgc_parameters(i,4)
                   paramE=fgc_parameters(i,5)
                   paramF=fgc_parameters(i,6)
                   fswitch = 1.0d0 / (1.0d0 + paramF * (paramE / r)**12)
                   ppair=fswitch*(paramA*exp(-paramB*r)+paramC/(r**paramD))
                   energy=energy+ppair
                endif
             enddo
          enddo

! Change units into kcal/mol used by MOPAC for the calculations

          fgc_energy = energy / 4.184

! Save some memory 

          deallocate(d, pp1, pp2, ind, jnd)
          close(90)
          return
       
! When the variable of the derivatives is present, 
! only those are calculated
       
    elseif (present(driv)) then

! As for the energy, the setup file is opened twice,
! the first to measure the length of the file, and 
! second to save all data from it

    open(94, file="setup.txt", status="OLD")

    dd=0
    do
       read(94,*,iostat=stat) buf
       if (stat/=0) exit
       dd=dd+1
    enddo

    rewind(94)

    ddd=2*dd
    allocate(d(dd), pp1(ddd), pp2(ddd), ind(ddd), jnd(ddd))
    dri=0
    j=1
    do j=1, dd
       i=dd+j
       read(94,30)a,b,d(j),p1,p2
       ind(j) = a 
       jnd(j) = b
       pp1(j) = p1
       pp2(j) = p2
       ind(i) = b
       jnd(i) = a
       pp1(i) = p2
       pp2(i) = p1
    enddo

    rewind(94)

! Same loops as for the energy checking if the keyworks readed match 
! with some of the available parameters
! and then if they match the correction is calculated
! The difference is that here the derivatives with respect the energy are calculated 

    driv(:,:) = 0.d0
    do lo=1, numat
       do j=1, ddd 
          if (lo==ind(j)) then
             do i=1, n
                p1=fgc_symbols(i,1)
                p2=fgc_symbols(i,2)
                   if (p1==pp1(j) .and. p2==pp2(j)) then
                      paramA=fgc_parameters(i,1)
                      paramB=fgc_parameters(i,2)
                      paramC=fgc_parameters(i,3)
                      paramD=fgc_parameters(i,4)
                      paramE=fgc_parameters(i,5)
                      paramF=fgc_parameters(i,6)
                      fswitch = 1.0d0 / (1.0d0 + paramF * (paramE / r)**12)
                      r=sqrt((x(lo)-x(jnd(j)))**2+(y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2)
                      dVr=-fswitch * ((paramA * paramB * exp(-paramB * r))+(paramC * paramD * r**(-paramD - 1)))+ &
            (12 * fswitch**2 * paramF * (paramE**12/ r**3)) * ((paramA * exp(-paramB * r)) + (paramC / r**paramD))
                      drx=((x(lo)-x(jnd(j)))/sqrt((x(lo)-x(jnd(j)))**2+ & 
		      (y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2))
                      dry=((y(lo)-y(jnd(j)))/sqrt((x(lo)-x(jnd(j)))**2+ & 
		      (y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2))
                      drz=((z(lo)-z(jnd(j)))/sqrt((x(lo)-x(jnd(j)))**2+ & 
		      (y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2))
                      driv(1,lo)=driv(1,lo)+((dVr*drx)/4.184)
                      driv(2,lo)=driv(2,lo)+((dVr*dry)/4.184)
                      driv(3,lo)=driv(3,lo)+((dVr*drz)/4.184)
                   elseif (p2==pp1(j) .and. p1==pp2(j)) then
                      paramA=fgc_parameters(i,1)
                      paramB=fgc_parameters(i,2)
                      paramC=fgc_parameters(i,3)
                      paramD=fgc_parameters(i,4)
                      paramE=fgc_parameters(i,5)
                      paramF=fgc_parameters(i,6)
                      fswitch = 1.0d0 / (1.0d0 + paramF * (paramE / r)**12)
                      r=sqrt((x(lo)-x(jnd(j)))**2+(y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2)
                      dVr=-fswitch * ((paramA * paramB * exp(-paramB * r))+(paramC * paramD * r**(-paramD - 1)))+ &
            (12 * fswitch**2 * paramF * (paramE**12/ r**3)) * ((paramA * exp(-paramB * r)) + (paramC / r**paramD))
                      drx=((x(lo)-x(jnd(j)))/sqrt((x(lo)-x(jnd(j)))**2+ &
		      (y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2))
                      dry=((y(lo)-y(jnd(j)))/sqrt((x(lo)-x(jnd(j)))**2+ &
		      (y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2))
                      drz=((z(lo)-z(jnd(j)))/sqrt((x(lo)-x(jnd(j)))**2+ & 
		      (y(lo)-y(jnd(j)))**2+(z(lo)-z(jnd(j)))**2))
                      driv(1,lo)=driv(1,lo)+((dVr*drx)/4.184)
                      driv(2,lo)=driv(2,lo)+((dVr*dry)/4.184)
                      driv(3,lo)=driv(3,lo)+((dVr*drz)/4.184)
                   endif
                enddo
             endif
          enddo
       enddo


       deallocate(d, pp1, pp2, ind, jnd)
       close(94)
       return

    endif

    return

end subroutine fgc

