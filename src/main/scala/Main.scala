import java.io.{File, PrintWriter}

import org.openrdf.model.vocabulary.RDFS
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.io.{FileDocumentTarget, StringDocumentTarget}
import org.semanticweb.owlapi.model.{AddImport, IRI, OWLAxiom, OWLIndividual}

import scala.collection.JavaConversions._
import scala.io.Source

object Main {
  val man = OWLManager.createOWLOntologyManager
  val ont = man.loadOntologyFromOntologyDocument(getClass.getResourceAsStream("/synth_eco_individuals.owl"))
  val df = man.getOWLDataFactory

  val fileStem = "/2010_ver1_12001_"  //change to "/2010_ver1_42003_" to process Miami-Dade county instead of Alachua County

  val IRIPrefix = "http://purl.obolibrary.org/obo/SYNECO_"
  var IRIEnding = 0

  var households = Map[String, OWLIndividual]()
  var housingUnits = Map[String, OWLIndividual]()

  var schoolOrgs = Map[String, OWLIndividual]()
  var schoolFacs = Map[String, OWLIndividual]()

  var workplaceOrgs = Map[String, OWLIndividual]()
  var workplaceFacs = Map[String, OWLIndividual]()

  var householders = Map[String, OWLIndividual]()

  var ageSpecs = Map[String, OWLIndividual]()

  def main(args: Array[String]) {
    val peopleRaw = Source.fromInputStream(getClass.getResourceAsStream(fileStem + "synth_people.txt"))

    var peopleProcessed = 0

    peopleRaw.getLines().foreach(personRaw => {
      val personFields = personRaw.split(",")
      val fieldCount = personFields.size
      val personID = personFields(0)
      val householdID = personFields(1)
      val relation = personFields(8)
      val race = personFields(6)
      val gender = personFields(5)
      val age = personFields(4)

      val workplaceID = if (fieldCount >= 11) personFields(10) else "" //if there is no workplace id, personFields is too small

      val schoolID = if (fieldCount >= 10) personFields(9) else "" //if there is no workplace id or school id, personFields is too small

      val axioms = buildPersonAxioms(personID, householdID, race, gender, workplaceID, schoolID, relation, age)
      man.addAxioms(ont, axioms)

      peopleProcessed += 1
      if (peopleProcessed % 10000 == 0) {
        println("Processed " + peopleProcessed + " people")
      }
    })

    val workplacesRaw = Source.fromInputStream(getClass.getResourceAsStream(fileStem + "workplaces.txt"))

    var workplacesProcessed = 0

    workplacesRaw.getLines().foreach(workplaceRaw => {
      val wpFields = workplaceRaw.split(",")
      val workplaceID = wpFields(0)
      val lat = wpFields(2)
      val long = wpFields(3)

      val workplaceEntity = workplaceFacs(workplaceID)

      val axioms = buildLatLongAxioms(workplaceEntity, "workplace facility " + workplaceID, lat, long)
      man.addAxioms(ont, axioms)

      workplacesProcessed += 1
      if (workplacesProcessed % 1000 == 0) {
        println("Processed " + workplacesProcessed + " workplaces")
      }
    })

    val schoolsRaw = Source.fromInputStream(getClass.getResourceAsStream(fileStem + "schools.txt"))

    var schoolsProcessed = 0

    schoolsRaw.getLines().foreach(schoolRaw => {
      val sFields = schoolRaw.split(",")
      val schoolID = sFields(0)
      val lat = sFields(14)
      val long = sFields(15)

      val schoolEntity = schoolFacs(schoolID)

      val axioms = buildLatLongAxioms(schoolEntity, "school facility " + schoolID, lat, long)
      man.addAxioms(ont, axioms)

      schoolsProcessed += 1
      if (schoolsProcessed % 10 == 0) {
        println("Processed " + schoolsProcessed + " schools")
      }
    })

    val householdsRaw = Source.fromInputStream(getClass.getResourceAsStream(fileStem + "synth_households.txt"))

    var householdsProcessed = 0

    householdsRaw.getLines().foreach(householdRaw => {
      val hFields = householdRaw.split(",")
      val householdID = hFields(0)
      val lat = hFields(7)
      val long = hFields(8)

      val houseEntity = housingUnits(householdID)

      val axioms = buildLatLongAxioms(houseEntity, "housing unit " + householdID, lat, long)
      man.addAxioms(ont, axioms)

      householdsProcessed += 1
      if (householdsProcessed % 10000 == 0) {
        println("Processed " + householdsProcessed + " households")
      }
    })

    println("saving file")
    val out = new FileDocumentTarget(new File("output/out.owl"))

    man.saveOntology(ont, out)

  }

  def buildLatLongAxioms(placeEntity: OWLIndividual, label: String, latValue: String, longValue: String): Set[OWLAxiom] = {
    val latDatumIRI = "http://purl.obolibrary.org/obo/OBI_0001620"
    val longDatumIRI = "http://purl.obolibrary.org/obo/OBI_0001621"
    val isAboutIRI = "http://purl.obolibrary.org/obo/IAO_0000136"
    val hasValueSpecIRI = "http://purl.obolibrary.org/obo/OBI_0001938"
    val valueSpecIRI = "http://purl.obolibrary.org/obo/OBI_0001933"
    val hasUnitLabelIRI = "http://purl.obolibrary.org/obo/IAO_0000039"
    val hasValueIRI = "http://purl.obolibrary.org/obo/IAO_0000004"
    val degreeIRI = "http://purl.obolibrary.org/obo/GEO_000000790"

    val (lat, laAxs) = createIndividual(latDatumIRI, "latitude of " + label)
    val (long, loAxs) = createIndividual(longDatumIRI, "longitude of " + label)
    val (longValueSpec, lovsAxs) = createIndividual(valueSpecIRI, longValue + " degree value specification")
    val (latValueSpec, lavsAxs) = createIndividual(valueSpecIRI, latValue + " degree value specification")

    var axs = laAxs ++ loAxs ++ lovsAxs ++ lavsAxs

    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isAboutIRI)), lat, placeEntity)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isAboutIRI)), long, placeEntity)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasValueSpecIRI)), lat, latValueSpec)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasUnitLabelIRI)), latValueSpec, df.getOWLNamedIndividual(IRI.create(degreeIRI)))
    axs += df.getOWLDataPropertyAssertionAxiom(df.getOWLDataProperty(IRI.create(hasValueIRI)), latValueSpec, df.getOWLLiteral(latValue.toFloat))
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasValueSpecIRI)), long, longValueSpec)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasUnitLabelIRI)), longValueSpec, df.getOWLNamedIndividual(IRI.create(degreeIRI)))
    axs += df.getOWLDataPropertyAssertionAxiom(df.getOWLDataProperty(IRI.create(hasValueIRI)), longValueSpec, df.getOWLLiteral(longValue.toFloat))

    axs
  }

  def buildPersonAxioms(personID: String, householdID: String, race: String, gender: String, workplaceID: String, schoolID: String, relation: String, age: String): Set[OWLAxiom] = {
    val humanIRI = "http://purl.obolibrary.org/obo/NCBITaxon_9606"

    var axioms = Set[OWLAxiom]()


    val (personEntity, personAxioms) = createIndividual(humanIRI, "person " + personID)
    axioms ++= personAxioms

    val (householdEntity, householdAxioms) = createHousehold(personEntity, householdID)
    axioms ++= householdAxioms

    if(schoolID != "") {
      val (schoolEntity, schoolAxioms) = createSchool(personEntity, schoolID, personID)
      axioms ++= schoolAxioms
    }

    if(workplaceID != "") {
      val (workplaceEntity, workplaceAxioms) = createWorkplace(personEntity, workplaceID, personID)
      axioms ++= workplaceAxioms
    }

    val (personRace, raceAxioms) = createPersonRace(personEntity, race, personID)
    axioms ++= raceAxioms

    val (personGender, genderAxioms) = createPersonGender(personEntity, gender, personID)
    axioms ++= genderAxioms

    val (personRelation, relationAxioms) = createPersonRelationship(personEntity, relation, personID, householdID)
    axioms ++= relationAxioms

    val (personAge, ageAxioms) = createPersonAge(personEntity, age, personID)
    axioms ++= ageAxioms

    return axioms
  }

  def createPersonAge(personEntity: OWLIndividual, age: String, personID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val activelyParticipatesIRI = "http://purl.obolibrary.org/obo/RO_0002217"
    val historyIRI = "http://purl.obolibrary.org/obo/BFO_0000182"
    val startsWithIRI = "http://purl.obolibrary.org/obo/RO_0002224"
    val birthIRI = "http://www.ifomis.org/acgt/1.0/#Birth"
    val occupiesIRI = "http://purl.obolibrary.org/obo/BFO_0000155"
    val oneDTempRegionIRI = "http://purl.obolibrary.org/obo/BFO_0000038"
    val hasLastInstantIRI = "http://purl.obolibrary.org/obo/SYNTH_ECO_00000456"
    val zeroDTempRegionIRI = "http://purl.obolibrary.org/obo/BFO_0000148"
    val ageDatumIRI = "http://purl.obolibrary.org/obo/SYNTH_ECO_00000433"
    val isAboutIRI = "http://purl.obolibrary.org/obo/IAO_0000136"
    val hasValueSpecIRI = "http://purl.obolibrary.org/obo/OBI_0001938"
    val valueSpecIRI = "http://purl.obolibrary.org/obo/OBI_0001933"
    val hasUnitLabelIRI = "http://purl.obolibrary.org/obo/IAO_0000039"
    val yearIRI = "http://purl.obolibrary.org/obo/SYNTH_ECO_00000432"
    val hasValueIRI = "http://purl.obolibrary.org/obo/IAO_0000004"
    val censusDateIRI = "http://purl.obolibrary.org/obo/SYNTH_ECO_00000457"

    val (history, hAxs) = createIndividual(historyIRI, "history of person " + personID)
    val (birth, bAxs) = createIndividual(birthIRI, "birth of person " + personID)
    val (interval, itAxs) = createIndividual(oneDTempRegionIRI, "temporal interval of history of person " + personID)

    val (ageDatum, aAxs) = createIndividual(ageDatumIRI, "age measurement datum at last temporal instance of temporal interval of history of person  " + personID)
    val (valueSpec, vAxs) = if(ageSpecs.contains(age)) {
      (ageSpecs(age), Set[OWLAxiom]())
    } else {
      val (v, a) = createIndividual(valueSpecIRI, age + " year value specification")
      ageSpecs += (age -> v)

      (v, a)
    }


    var axs = hAxs ++ bAxs ++ itAxs ++ aAxs ++ vAxs

    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(activelyParticipatesIRI)), personEntity, history)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(startsWithIRI)), history, birth)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(occupiesIRI)), history, interval)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(activelyParticipatesIRI)), personEntity, history)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasLastInstantIRI)), interval, df.getOWLNamedIndividual(IRI.create(censusDateIRI)))
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isAboutIRI)), ageDatum, interval)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasValueSpecIRI)), ageDatum, valueSpec)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasUnitLabelIRI)), valueSpec, df.getOWLNamedIndividual(IRI.create(yearIRI)))
    axs += df.getOWLDataPropertyAssertionAxiom(df.getOWLDataProperty(IRI.create(hasValueIRI)), valueSpec, df.getOWLLiteral(age))

    (history, axs)
  }

  def createPersonRelationship(personEntity: OWLIndividual, relationCode: String, personID: String, householdID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val marriageContractIRI = "http://purl.obolibrary.org/obo/OMRSE_00000005"
    val householderIRI = "http://purl.obolibrary.org/obo/OMRSE_00000081"
    val collectionOfHumansIRI = "http://purl.obolibrary.org/obo/OMRSE_00000023"
    val isBearerOfIRI = "http://purl.obolibrary.org/obo/RO_0000053"

    val relationIRI = relationCode match {
      case "1" => marriageContractIRI
      case "0" => householderIRI
      case _ => ""
    }

    val (relation, axioms) = if(relationIRI == marriageContractIRI){
      var (relation, axioms) = createIndividual(relationIRI, "party to a marriage contract role for person " + personID)
      axioms += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isBearerOfIRI)), personEntity, relation)
      if(householders.contains(householdID)){
        val householder = householders(householdID)
        axioms += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isBearerOfIRI)), householder, relation) //TODO: Add householder ID to role somehow, too
      }
      (relation, axioms)
    } else if (relationIRI == householderIRI){
      var (relation, axioms) = createIndividual(relationIRI, "householder role of person " + personID)
      axioms += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isBearerOfIRI)), personEntity, relation)
      householders += (householdID -> personEntity)
      (relation, axioms)
    } else {
      (df.getOWLNamedIndividual(IRI.create(collectionOfHumansIRI)), Set[OWLAxiom]()) //throwaway
    }

    (relation, axioms)
  }

  def createPersonGender(personEntity: OWLIndividual, genderCode: String, personID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val maleIRI = "http://purl.obolibrary.org/obo/OMRSE_00000008"
    val femaleIRI = "http://purl.obolibrary.org/obo/OMRSE_00000009"
    val collectionOfHumansIRI = "http://purl.obolibrary.org/obo/OMRSE_00000023"
    val isBearerOfIRI = "http://purl.obolibrary.org/obo/RO_0000053"

    val genderIRI = genderCode match {
      case "1" => maleIRI
      case "2" => femaleIRI
      case _ => ""
    }

    val (gender, axioms) = if(genderIRI != ""){
      var (genderRole, axs) = createIndividual(genderIRI, "gender role of person " + personID)
      axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isBearerOfIRI)), personEntity, genderRole)
      (genderRole, axs)
    } else {
      (df.getOWLNamedIndividual(IRI.create(collectionOfHumansIRI)), Set[OWLAxiom]()) //throwaway
    }

    (gender, axioms)
  }

  def createPersonRace(personEntity: OWLIndividual, raceCode: String, personID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val blackIRI = "http://purl.obolibrary.org/obo/omrse/dao/African_American_or_Black_Origin"
    val indianIRI = "http://purl.obolibrary.org/obo/omrse/dao/American_Indian_or_Alaskan_Native_Origin"
    val asianIRI = "http://purl.obolibrary.org/obo/omrse/dao/Asian_Origin"
    val whiteIRI = "http://purl.obolibrary.org/obo/omrse/dao/Caucasian_or_White_Origin"
    val islanderIRI = "http://purl.obolibrary.org/obo/omrse/dao/Hawaiian_Native_or_Pacific_Islander_Origin"
    val collectionOfHumansIRI = "http://purl.obolibrary.org/obo/OMRSE_00000023"
    val isInAggregateIRI = "http://purl.obolibrary.org/obo/GEO_000000328"

    val raceIRI = raceCode match {
      case "1" => whiteIRI
      case "2" => blackIRI
      case "3" => indianIRI
      case "4" => indianIRI
      case "5" => indianIRI
      case "6" => asianIRI
      case "7" => islanderIRI
      case _ => ""
    }

    val (race, axioms) = if(raceIRI != ""){
      val race = df.getOWLNamedIndividual(IRI.create(raceIRI))
      val axs = Set[OWLAxiom](df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isInAggregateIRI)), personEntity, race))
      (race, axs)
    } else {
      (df.getOWLNamedIndividual(IRI.create(collectionOfHumansIRI)), Set[OWLAxiom]())
    }

    (race, axioms)
  }

  def createWorkplace(personEntity: OWLIndividual, workplaceID: String, personID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val employeeRoleIRI = "http://purl.obolibrary.org/obo/OMRSE_00000077"
    val processIRI = "http://purl.obolibrary.org/obo/BFO_0000015"
    val facilityIRI = "http://purl.obolibrary.org/obo/OMRSE_00000078"
    val unfoldsIRI = "http://purl.obolibrary.org/obo/BFO_0000066"
    val realizedByIRI = "http://purl.org/obo/owl/OBO_REL#realized_by"
    val slgdcIRI = "http://purl.obolibrary.org/obo/IAO_0021004" //socio-legal generic dependent continuant
    val declarationIRI = "http://purl.obolibrary.org/obo/IAO_0021005"
    val workplaceOrgIRI = "http://purl.obolibrary.org/obo/OBI_0000245"
    val isConcretizationOfIRI = "http://purl.obolibrary.org/obo/RO_0000059"
    val isSpecifiedOutputOfIRI = "http://purl.obolibrary.org/obo/OBI_0000312"
    val hasActiveParticipantIRI = "http://purl.obolibrary.org/obo/RO_0002218"
    val administratesIRI = "http://purl.obolibrary.org/obo/OMIABIS_0000009"

    val isAboutIRI = "http://purl.obolibrary.org/obo/IAO_0000136"
    val isBearerOfIRI = "http://purl.obolibrary.org/obo/RO_0000053"
    val participatesIRI = "http://purl.obolibrary.org/obo/RO_0000056"


    val (org: OWLIndividual, facility: OWLIndividual, wAxs: Set[OWLAxiom]) = if(workplaceOrgs.contains(workplaceID)){
      val org = workplaceOrgs(workplaceID)
      val facility = workplaceFacs(workplaceID)
      (org, facility, Set[OWLAxiom]())
    } else {
      //generating workplace things only once
      val (facility, fAxs) = createIndividual(facilityIRI, "workplace facility " + workplaceID)

      val (org, oAxs) = createIndividual(workplaceOrgIRI, "workplace organization " + workplaceID)

      var wAxs = fAxs ++ oAxs

      wAxs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(administratesIRI)), org, facility)


      workplaceOrgs += (workplaceID -> org)
      workplaceFacs += (workplaceID -> facility)

      (org, facility, wAxs)
    }

    //generating the person-level things
    val (role, roAxs) = createIndividual(employeeRoleIRI, "employee role of person " + personID)
    val (realization, reAxs) = createIndividual(processIRI, "realization of employee role of person " + personID)
    val (slgdc, slAxs) = createIndividual(slgdcIRI, "socio-legal generic dependent continuant of employee " + personID)
    val (declare, dAxs) = createIndividual(declarationIRI, "declaration of socio-legal generic dependent continuant of employee " + personID)


    var axs = wAxs ++ roAxs ++ reAxs ++ slAxs ++ dAxs

    //relating the things to each other
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(unfoldsIRI)), realization, facility)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(realizedByIRI)), role, realization)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isConcretizationOfIRI)), role, slgdc)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isSpecifiedOutputOfIRI)), slgdc, declare)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasActiveParticipantIRI)), declare, org)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(participatesIRI)), personEntity, realization)


    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isBearerOfIRI)), personEntity, role)

    return (org, axs)
  }

  def createSchool(personEntity: OWLIndividual, schoolID: String, personID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val studentRoleIRI = "http://purl.obolibrary.org/obo/OMRSE_00000058"
    val processIRI = "http://purl.obolibrary.org/obo/BFO_0000015"
    val facilityIRI = "http://purl.obolibrary.org/obo/OMRSE_00000064"
    val unfoldsIRI = "http://purl.obolibrary.org/obo/BFO_0000066"
    val realizedByIRI = "http://purl.org/obo/owl/OBO_REL#realized_by"
    val slgdcIRI = "http://purl.obolibrary.org/obo/IAO_0021004" //socio-legal generic dependent continuant
    val declarationIRI = "http://purl.obolibrary.org/obo/IAO_0021005"
    val schoolOrgIRI = "http://purl.obolibrary.org/obo/OMRSE_00000057"
    val isConcretizationOfIRI = "http://purl.obolibrary.org/obo/RO_0000059"
    val isSpecifiedOutputOfIRI = "http://purl.obolibrary.org/obo/OBI_0000312"
    val hasActiveParticipantIRI = "http://purl.obolibrary.org/obo/RO_0002218"
    val administratesIRI = "http://purl.obolibrary.org/obo/OMIABIS_0000009"
    val isAboutIRI = "http://purl.obolibrary.org/obo/IAO_0000136"
    val isBearerOfIRI = "http://purl.obolibrary.org/obo/RO_0000053"
    val participatesIRI = "http://purl.obolibrary.org/obo/RO_0000056"

    val (org: OWLIndividual, facility: OWLIndividual, sAxs: Set[OWLAxiom]) = if(schoolOrgs.contains(schoolID)){
      val org = schoolOrgs(schoolID)
      val facility = schoolFacs(schoolID)
      (org, facility, Set[OWLAxiom]())
    } else {
      //generating school things only once
      val (facility, fAxs) = createIndividual(facilityIRI, "school facility " + schoolID)
      val (org, oAxs) = createIndividual(schoolOrgIRI, "school organization " + schoolID)

      var sAxs = fAxs ++ oAxs

      sAxs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(administratesIRI)), org, facility)

      schoolOrgs += (schoolID -> org)
      schoolFacs += (schoolID -> facility)

      (org, facility, sAxs)
    }

    //generating the person-level things
    val (role, roAxs) = createIndividual(studentRoleIRI, "student role of person " + personID)
    val (realization, reAxs) = createIndividual(processIRI, "realization of student role of person " + personID)
    val (slgdc, slAxs) = createIndividual(slgdcIRI, "socio-legal generic dependent continuant of student " + personID)
    val (declare, dAxs) = createIndividual(declarationIRI, "declaration of socio-legal generic dependent continuant of student " + personID)


    var axs = sAxs ++ roAxs ++ reAxs ++ slAxs ++ dAxs

    //relating the things to each other
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(unfoldsIRI)), realization, facility)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(realizedByIRI)), role, realization)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isConcretizationOfIRI)), role, slgdc)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isSpecifiedOutputOfIRI)), slgdc, declare)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(hasActiveParticipantIRI)), declare, org)
    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(participatesIRI)), personEntity, realization)

    axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isBearerOfIRI)), personEntity, role)

    return (org, axs)
  }

  def createHousehold(personEntity: OWLIndividual, householdID: String): (OWLIndividual, Set[OWLAxiom]) = {
    val householdIRI = "http://purl.obolibrary.org/obo/OMRSE_00000076"
    val participatesIRI = "http://purl.obolibrary.org/obo/RO_0000056"
    val activelyParticipatesIRI = "http://purl.obolibrary.org/obo/RO_0002217"
    val processIRI = "http://purl.obolibrary.org/obo/BFO_0000015"
    val unfoldsIRI = "http://purl.obolibrary.org/obo/BFO_0000066"
    val housingUnitIRI = "http://purl.obolibrary.org/obo/OMRSE_00000074"
    val isInAggregateIRI = "http://purl.obolibrary.org/obo/GEO_000000328"
    val isAboutIRI = "http://purl.obolibrary.org/obo/IAO_0000136"

    var (household, axioms) = if (households.contains(householdID)) {
      (households(householdID), Set[OWLAxiom]())
    } else {
      val (h, hAxs) = createIndividual(householdIRI, "household " + householdID)
      val (realization, rAxs) = createIndividual(processIRI, "realization of residence function of housing unit " + householdID)
      val (unit, uAxs) = createIndividual(housingUnitIRI, "housing unit " + householdID)

      var axs = hAxs ++ rAxs ++ uAxs

      axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(participatesIRI)), h, realization)
      axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(unfoldsIRI)), realization, unit)
      axs += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(activelyParticipatesIRI)), unit, realization)

      households += (householdID -> h)
      housingUnits += (householdID -> unit)

      (h, axs)
    }

    axioms += df.getOWLObjectPropertyAssertionAxiom(df.getOWLObjectProperty(IRI.create(isInAggregateIRI)), personEntity, household)

    return (household, axioms)
  }

  def createIndividual(typeIRI: String, label: String): (OWLIndividual, Set[OWLAxiom]) = {
    val indiv = df.getOWLNamedIndividual(getIRI())
    val cls = df.getOWLClass(IRI.create(typeIRI))

    val axioms = Set(
      df.getOWLClassAssertionAxiom(cls, indiv),
      df.getOWLAnnotationAssertionAxiom(
        df.getOWLAnnotationProperty(IRI.create(RDFS.LABEL.toString)),
        indiv.getIRI,
        df.getOWLLiteral(label)
      )
    )


    return (indiv, axioms)
  }

  def getIRI(): IRI = {
    this.IRIEnding += 1
    return IRI.create(this.IRIPrefix + "%08d".format(this.IRIEnding))
  }
}
