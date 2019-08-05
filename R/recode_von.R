#' VON recoding function
#'
#' This function takes a VON export converted into a dataframe, and recodes the variables.
#' @param von A dataframe output of a VON data export
#' @keywords VON
#' @export
#' @examples
#' # recode_von()

recode_von = function ( von ) {

    recode2 = function ( data, fields, recodes, as.factor = FALSE ) {
        for ( i in which(names(data) %in% fields) ) { # iterate over column indexes that are present in the passed dataframe that are also included in the fields list
            data[,i] <- car::recode( data[,i], recodes, as.factor = as.factor )
        }
        data
    }

    # 7, 77, 777, 777.77, 77777 codes should generally be Unknown, not NA

    names(von) <- tolower(names(von))

    von <- recode2( data = von,
                    fields = c( "cmal","deldie","pcare","aster","amagsulf","chorio","mhypertens",
                                "lbpath","nec","necsurg","ox36","ropsurg","mult","drox","drbm","dret",
                                "drep","drcc","drcpap","atempm","ebseps","newox28","usound1","die12",
                                "oxy","vent","hfv","hfnc","nimv","cpap","cpapes","drsurf","surfx","ino",
                                "ox36","vent36","hfv36","hfnc36","numv36","cpap36","sterbpd",
                                "indometh","ibuprofen","probiotics","ropantivegf","srglig","osurg",
                                "rds","pntx","pda","giperf","cnegstaph","fungal","pvl","eyex",
                                "oxfinal","acfinal","ecmop","cooled","hypoiep","mecasp","trcsucma","seizure","nimv36"),
                    recodes = "0 = 'No'; 1 = 'Yes'; 7 = NA; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "istage","ugrade1" ),
                    recodes = "7 = NA; 9 = NA" )

    von <- recode2( data = von,
                    fields = c( "gadays","gaweeks","nbirths","ap1","ap5","surf1dmin","bdefect","osrgdesc" ),
                    recodes = "77 = NA; 99 = NA" )

    von <- recode2( data = von,
                    fields = c( "dayadmiss" ),
                    recodes = "77 = NA" ) # recoding day of admission for inborns and unknown to NA

    von <- recode2( data = von,
                    fields = c( "los1","lostot" ),
                    recodes = "777 = NA; 999 = NA" )

    von <- recode2( data = von,
                    fields = c( "bheadcir","dheadcir","atemp" ),
                    recodes = "777.7 = NA; 999.9 = NA" )

    von <- recode2( data = von,
                    fields = c( "surf1dhr","ventdays" ),
                    recodes = "7777 = NA; 9999 = NA" )

    von <- recode2( data = von,
                    fields = c( "bwgt","dwgt","f3wgt" ),
                    recodes = "77777 = NA; 99999 = NA" )

    von <- recode2( data = von,
                    fields = c( "transcode" ),
                    recodes = "0='ECMO'; 1='Growth/ Discharge Planning'; 2='Medical/Diagnostic Services'; 3='Surgery'; 4='Chronic Care'; 5='Other'; 7=NA; 9=NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "fdisp" ),
                    recodes = "1 = 'Home'; 2 = 'Transfer'; 3 = 'Died'; 5='Still Hospitalized as of First Birthday'; 7 = NA; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "f2disp" ),
                    recodes = "1='Home'; 2='Transferred Again'; 3='Died'; 4='Readmitted'; 5='Still Hospitalized as of First Birthday'; 7=NA; 9=NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "f3disp","udisp" ),
                    recodes = "1 = 'Home'; 2 = 'Transfer'; 3 = 'Died'; 5='Still Hospitalized as of First Birthday'; 7 = NA; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "locate" ),
                    recodes = "0 = 'Inborn'; 1 = 'Outborn'", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "sex" ),
                    recodes = "0 = 'Female'; 1 = 'Male'; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "vagdel" ),
                    recodes = "0 = 'C-section'; 1 = 'Vaginal'; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "hisp" ),
                    recodes = "0 = 'Not Hispanic'; 1 = 'Hispanic'; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "matrace" ),
                    recodes = "1='Black or African American'; 3='White'; 4='Asian'; 5='American Indian or Alaska Native'; 6='Native Hawaiian or Other Pacific Islander'; 7='Other Race'; 99='Unknown'", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "piewfo","inowg","sterbpdwg","ropsurgwd","srgligwd",
                                "srgloc1","srgloc2","srgloc3","srgloc4","srgloc5","srgloc6","srgloc7","srgloc8","srgloc9","srgloc10",
                                "pntxwo","necwo","giperfwo","lbpathwo","cnegwo","fungalwo"),
                    recodes = "1='Your Hospital'; 2='Other Hospital'; 3='Both Your Hospital and Other Hospital'; 7=NA; 9=NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "entfeed" ),
                    recodes = "0='None'; 1='Human Milk Only'; 2='Formula Only';3='Human Milk with Fortifier or Formula'; 7 = NA; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "durvent" ),
                    recodes = "0='None'; 1='<4 Hours'; 2='4 to 24 hours'; 3='>24 Hours'; 7 = NA; 9 = NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "coolmeth" ),
                    recodes = "1='Selective Head'; 2='Whole Body'; 3='Both Selective Head and Whole Body'; 7=NA; 9=NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "hypoies" ),
                    recodes = "1='Mild'; 2='Moderate'; 3='Severe'; 7=NA; 9=NA", as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c( "srgcd1","srgcd2","srgcd3","srgcd4","srgcd5","srgcd6","srgcd7","srgcd8","srgcd9","srgcd10" ),
                    recodes = paste0("77=NA;99='Unknown';'S415'='???';'S101'='Tracheostomy/Tracheotomy';'S102'='Cricoid split';'S103'='Ophthalmologic surgery OTHER THAN laser or cryosurgery for ROP';'S104'='Cleft lip or palate repair';",
                         "'S105'='Branchial cleft sinus excision';'S106'='Thyroglossal duct excision';'S107'='Palliative or definitive repair of choanal atresia';'S108'='Mandibular (jaw) distraction';'S109'='Craniotomy';",
                         "'S100'='Other head and neck surgery requiring general or spinal anesthesia';'S201'='Tracheal Resection';'S202'='Aortopexy';'S203'='Tracheoesophageal atresia and/or fistula repair';",
                         "'S204'='Thoracoscopy (with or without pleuridesis or pleurectomy)';'S205'='Thoracotomy (with or without pleural or lung biopsy)';'S206'='Thoracotomy (or thoracoscopy) with pneumonectomy, lobectomy or partial lobectomy';",
                         "'S207'='Resection of pulmonary sequestration (intrathoracic or extrathoracic)';'S208'='Resection of mediastinal mass';'S209'='Resection of chest wall';'S210'='Bronchoscopy (with or without biopsy)';",
                         "'S211'='Esophagoscopy (with or without biopsy)';'S212'='Surgery for Congenital Cystic Adenomatoid Malformation of the Lung';'S213'='Lung transplant';'S214'='Sternal closure';'S200'='Other thoracic surgery requiring general or spinal anesthesia';",
                         "'S301'='Rectal biopsy with or without anoscopy';'S302'='Laparoscopy (diagnostic, with/without biopsy)';'S303'='Laparotomy (diagnostic or exploratory, with/without biopsy)';'S304'='Fundoplication';'S305'='Pyloromyotomy';",
                         "'S306'='Pyloroplasty';'S307'='Jejunostomy, ileostomy, enterostomy, colostomy for intestinal diversion (with or without bowel resection, with or without fistula creation)';'S308'='Small bowel resection with or without primary anastomosis';",
                         "'S309'='Large bowel resection';'S310'='Duodenal atresia/stenosis/web repair';'S311'='Jejunal, ileal, or colonic atresia repair (or repair of multiple intestinal atresias)';'S312'='Excision of Meckels diverticulum';",
                         "'S313'='Drainage of intra-abdominal abscess (not as primary treatment for NEC, see code S 333)';'S314'='Surgery for meconium ileus';'S315'='Excision of omphalomesenteric duct or duct remnant';",
                         "'S316'='Gastroschisis repair (primary or staged)';'S317'='Omphalocele repair (primary or staged)';'S318'='Lysis of adhesions';'S319'='Repair of imperforate anus (with or without vaginal, urethral, or vesicle fistula)';",
                         "'S320'='Pull through for Hirschsprungs disease (any technique)';'S321'='Pancreatectomy (partial, near total or total)';'S322'='Splenectomy or splenorrhaphy (partial or complete)';'S323'='Resection of retroperitoneal tumor';",
                         "'S324'='Resection of sacrococcygeal tumor';'S325'='Repair of diaphragmatic hernia';'S326'='Plication of the diaphragm';'S327'='Gastrostomy/jejunostomy tube';'S328'='Upper endoscopy (stomach or duodenum, with or without biopsy)';",
                         "'S329'='Colonoscopy/sigmoidoscopy (with or without biopsy)';'S330'='Takedown of ostomy and/or reanastomosis of bowel (small or large bowel)';'S331'='Ladds or other procedure for correction of malrotation';'S332'='Appendectomy';",
                         "'S333'='Primary peritoneal drainage for NEC, suspected NEC or intestinal perforation';'S334'='Anoplasty';'S335'='Kasai procedure';'S336'='Liver biopsy done during laparotomy or laparoscopy (includes wedge or needle techniques)';",
                         "'S337'='Umbilical hernia repair';'S300'='Other abdominal surgery requiring general or spinal anesthesia';'S401'='Cystoscopy (diagnostic, with or without biopsy)';'S402'='Adrenalectomy';'S403'='Nephrectomy';'S404'='Nephrostomy';",
                         "'S405'='Urteterostomy';'S406'='Resection of urachal cyst';'S407'='Cystostomy';'S408'='Closure of bladder exstrophy';'S409'='Resection of posterior urethral valves';'S410'='Inguinal hernia repair';'S411'='Orchiopexy';'S412'='Orchiectomy';",
                         "'S413'='Drainage, excision or removal of ovarian cyst';'S414'='Oopherectomy (partial or complete)';'S416'='Pyeloplasty';'S417'='Renal transplant';'S400'='Other genito-urinary surgery requiring general or spinal anesthesia';",
                         "'S501'='Vascular Ring division';'S502'='Repair of coarctation of the aorta';'S503'='Repair of major vascular injury';'S504'='Repair or palliation of congenital heart disease';'S505'='Heart transplant';",
                         "'S506'='Implanted pacemaker (permanent - do not use code for temporary pacemakers)';'S500'='Other open heart or vascular surgery requiring general or spinal anesthesia';'S601'='Diagnostic cardiac catheterization';",
                         "'S602'='Interventional catheterization with balloon septostomy';'S603'='Interventional catheterization with aortic valvuloplasty';'S604'='Interventional catheterization with pulmonary valvuloplasty';",
                         "'S600'='Other interventional catheterization whether or not anesthesia was required';'S700'='Skin or soft tissue surgery requiring general or spinal anesthesia';'S800'='Other musculoskeletal surgery requiring general or spinal anesthesia';",
                         "'S901'='Ventriculoperitoneal or other ventricular shunt';'S902'='External ventricular drain';'S903'='Ventricular drain with reservoir placement or removal';'S904'='Meningocele or myelomeningocele repair';'S905'='Encephalocele repair';",
                         "'S900'='Other central nervous system surgery requiring general or spinal anesthesia';'S1000'='Fetal surgery at your hospital';'S1001'='Fetal surgery at another hospital';'S1101'='Separation of conjoined twins'"),
                    as.factor = TRUE )

    von <- recode2( data = von,
                    fields = c("bdcd1","bdcd2","bdcd3","bdcd4","bdcd5"),
                    recodes = paste0("7777=NA; 9999='Unknown'; 101='Anencephaly'; 102='Meningomyelocele'; 103='Hydranencephaly'; 104='Congenital Hydrocephalus'; 105='Holoprosencephaly'; 901='Other lethal or life threatening central nervous system defects';",
                        "201='Truncus Arteriosus'; 202='Transposition of the Great Vessels'; 203='Tetralogy of Fallot'; 204='Single Ventricle'; 205='Double Outlet Right Ventricle'; 206='Complete Atrio-Ventricular Canal'; 207='Pulmonary Atresia';",
                        "208='Tricuspid Atresia'; 209='Hypoplastic Left Heart Syndrome'; 210='Interrupted Aortic Arch'; 211='Total Anomalous Pulmonary Venous Return'; 212='Penatalogy of Cantrell (Thoraco-Abdominal Ectopia Cordis)';",
                        "902='Other lethal or life threatening congenital heart defects'; 301='Cleft Palate'; 302='Tracheo-Esophageal Fistula'; 303='Esophageal Atresia'; 304='Duodenal Atresia'; 305='Jejunal Atresia'; 306='Ileal Atresia';",
                        "307='Atresia of Large Bowel or Rectum'; 308='Imperforate Anus'; 309='Omphalocele'; 310='Gastroschisis'; 311='Biliary Atresia'; 903='Other lethal or life threatening gastro-intestinal defects'; 401='Bilateral Renal Agenesis';",
                        "402='Bilateral Polycystic, Multicystic, or Dysplastic Kidneys'; 403='Obstructive Uropathy with Congenital Hydronephrosis'; 404='Exstrophy of the Urinary Bladder'; 904='Other lethal or life threatening Genito-Urinary defects';",
                        "501='Trisomy 13'; 502='Trisomy 18'; 503='Trisomy 21'; 504='Other Chromosomal Anomaly'; 505='Triploidy'; 601='Skeletal Dysplasia'; 602='Congenital Diaphragmatic Hernia';",
                        "603='Hydrops Fetalis with anasarca and one or more of the following: ascites, pleural effusion, pericardial effusion';",
                        "604='Oligohydramnios sequence including all 3 of the following: (1) Oligohydramnios documented by antenatal ultrasound 5 or more days prior to delivery, (2) evidence of fetal constraint on postnatal physical exam (such as Potters facies, contractures, or positional deformities of limbs), and (3) postnatal respiratory failure requiring endotracheal intubation and assisted ventilation.';",
                        "605='Inborn Error of Metabolism'; 606='Myotonic Dystrophy requiring endotracheal intubation and assisted ventilation'; 607='Conjoined Twins'; 608='Tracheal Agenesis or Atresia';",
                        "609='Thanatophoric Dysplasia Types 1 and 2'; 610='Hemoglobin Barts'; 701='Congenital Cystic Adenomatoid Malformation of the Lung'; 907='Other lethal or life threatening pulmonary malformation';",
                        "100='Other lethal or life threatening birth defects'"),
                        as.factor = TRUE )

    if ( 'dob' %in% names(von) ) {
        von$dob <- as.Date( von$dob, format = '%m/%d/%Y' )
    }

    if ( 'doa' %in% names(von) ) {
      von$doa <- as.Date( von$doa, format = '%m/%d/%Y' )
    }

    # Per VON definition, dayadmiss is '77' for inborns --> NA, but this is a pain for calculations, so just change this to first day of life
    von[ is.na(von$dayadmiss), 'dayadmiss' ] <- 1 # dayadmiss is NA for those who are inborn, so replace those all with 1 -- 0 is NOT used; similarly LOS1 has 1 as lowest value

    return (von)
}



#' VON XML recoding function
#'
#' This function takes a link to a VON XML export and coverts it into a dataframe and recodes the variables.
#' @param von_xml A link to the XML output of a VON data export
#' @keywords VON
#' @export
#' @examples
#' # recode_von_xml()

recode_von_xml = function ( von_xml ) {
  df <- XML::xmlParse(von_xml)  # read XML object
  df <- XML::xmlToDataFrame(df) # convert XML object to dataframe, all characters
  df <- as.data.frame(lapply(df, utils::type.convert, as.is = TRUE), stringsAsFactors = FALSE) # infer column types; as.is and stringsAsFactors to keep as characters, not factors
  return(peditools::recode_von(df))
}



