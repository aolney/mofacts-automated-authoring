module LongformQA

open System
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json 
open Thoth.Fetch

//for node compatibility
importSideEffects "isomorphic-fetch"

//Fable 2 transition 
let inline toJson x = Encode.Auto.toString(4, x)
let inline ofJson<'T> json = Decode.Auto.unsafeFromString<'T>(json)


type Document = 
    {
        Article : string
        Sections : string
        Text : string
        Score : float
    }

type AnswerDocuments =
    {
        answer : string
        documents : Document[]
    }

type Answer = { answer : string }

let endpoint = "http://127.0.0.1:5000/api/"
// let endpoint = "https://longform-qa.olney.ai/api/"

// NOTE: sometimes when we use anonymous records in the requests, they are rendered as empty payloads later, so we manually specify all requests
type AnswerRequest =  { question : string }
type AnswerContextRequest = { question : string ; context : string[] }
type WikipediaAnswerRequest = { question : string ; indexName : string }
type DocumentsRequest = { query : string }

/// Get a long form answer and associated support documents; defaulting to index ap_snippets_100w
let getAnswer(question: string): JS.Promise<Result<AnswerDocuments,FetchError>> =
    promise {
        return! Fetch.tryPost( endpoint + "getAnswer", { question=question;  } )// caseStrategy = SnakeCase) //capitalization on the document fields seems to blow up the case strategy
    }
/// Get a long form answer without associated documents, which we supplied; defaults to index as above
let getAnswerWithContext( question: string) (context : string[]): JS.Promise<Result<Answer,FetchError>> =
    promise {
        return! Fetch.tryPost( endpoint + "getAnswerWithContext", { question=question; context=context }, caseStrategy = SnakeCase)
    }
/// Get a long form answer and associated documents using the wikipedia index
let getAnswerWikipedia(question: string): JS.Promise<Result<AnswerDocuments,FetchError>> =
    promise {
        return! Fetch.tryPost( endpoint + "getAnswer", { question=question; indexName = "wiki40b_snippets_100w"  }, caseStrategy = CamelCase ) //SnakeCase)
    }
/// Get just the documents associated with the query
let getDocuments(query: string): JS.Promise<Result<Document[],FetchError>> =
    promise {
        return! Fetch.tryPost( endpoint + "getDocuments", { query=query;  } )// caseStrategy = SnakeCase) //capitalization on the document fields seems to blow up the case strategy
    }

let testAnswer( question : string) = 
    // getAnswer( question )
    // getAnswerWikipedia question
    // What is Jacobs syndrome? (approx)
    // getAnswerWithContext question [|"1 in every 500 to 2,000 male births. One male in 1,000 has an extra Y chromosome, called XYY syndrome, or Jacobs syndrome. A fertilized ovum that has one Y chromosome and no X chromosome has never been observed. Presumably, when a zygote lacks an X chromosome, so much genetic material is missing that only a few, if any, cell divisions are possible. Several types of tests performed on pregnant women can identify anatomical or physiological features of fetuses that are often found with a specific chromosomal problem or detect theabnormal chromosomes. An ultrasound scan, for example, can reveal the"|]
    // What is the relationship between the cerebrum and cerebellum?
    // getAnswerWithContext question [| "The pattern of these elevations and depressions is complex, but, despite individual variations, is similar in all normal brains. For example, a longitudinal fissure separates the right and left cerebral hemispheres; a transverse fissure separates the cerebrum from the cerebellum; and sulci divide each hemisphere into lobes. Most of the five lobes of the cerebral hemispheres are named after the skull bones that they underlie. The lobes of each hemisphere include the following: All lobes of the cerebrum have a thin layer of gray matter called the cerebral cortex. The cortex constitutes the outermost portion of the cerebrum. It covers"; "matter called the red nucleus. This nucleus communicates with the cerebellum and with centers of the spinal cord, and it plays a role in reflexes that maintain posture. It appears red because it is richly supplied with blood vessels. The pons occupies the full thickness of the brainstem, but it is most visible ventrally as a rounded bulge where it separates the midbrain from the medulla oblongata. The ventral portion of the pons consists mostly of longitudinal nerve fibers, which relay information between the medulla oblongata and the cerebrum. Its ventral portion also contains large bundles of transverse nerve fibers"; "that wrap around to the back and connect with the cerebellum. They conduct impulses from the cerebrum to centers within the cerebellum. Several nuclei of the pons relay sensory information from peripheral nerves to higher brain centers. Other nuclei may function with centers of the medulla oblongata to control breathing. The medulla oblongata is an enlarged continuation of the spinal cord, extending from the level of the foramen magnum to the pons. Its dorsal surface flattens to form the floor of the fourth ventricle, and its ventral surface is marked by two longitudinal enlargements called the pyramids. These contain descending"; "separated by a layer of dura mater called the falx cerebelli. A structure called the vermis connects the cerebellar hemispheres at the midline. Like the cerebrum, the cerebellum is primarily composed of white matter with a thin layer of gray matter, the cerebellar cortex, on its surface. This cortex doubles over on itself in a series of complex folds that have myelinated nerve fibers branching into them. A cut into the cerebellum reveals a treelike pattern of white matter, called the arbor vitae, surrounded by gray matter. A number of nuclei lie deep within each cerebellar hemisphere. The largest and"; "REM sleep sometimes twitch their limbs. In humans, REM sleep usually lasts from five to fifteen minutes. This \"dream sleep\" is important. If a person lacks REM sleep for just one night, sleep on the next night makes up for it. During REM sleep, heart and respiratory rates are irregular. Marijuana, alcohol, and certain other drugs, such as benzodiazepines, interfere with REM sleep. it describes several disorders of sleep. The cerebellum is a large mass of tissue inferior to the occipital lobes of the cerebrum and dorsal to the pons and medulla oblongata. It consists of two lateral hemispheres partially" |]
    // BEST: GLOSSARY SENTENCES AS FIRST DOCS, FOLLOWED BY ELASTIC DOCS WITH BOTH TERMS IN THEM
    // getAnswerWithContext question [| "The cerebrum is part of the brain in the upper part of the cranial cavity that provides higher mental functions." ; "The cerebellum is the part of the brain that coordinates skeletal muscle movement.";"The pattern of these elevations and depressions is complex, but, despite individual variations, is similar in all normal brains. For example, a longitudinal fissure separates the right and left cerebral hemispheres; a transverse fissure separates the cerebrum from the cerebellum; and sulci divide each hemisphere into lobes. Most of the five lobes of the cerebral hemispheres are named after the skull bones that they underlie. The lobes of each hemisphere include the following: All lobes of the cerebrum have a thin layer of gray matter called the cerebral cortex. The cortex constitutes the outermost portion of the cerebrum. It covers"; "matter called the red nucleus. This nucleus communicates with the cerebellum and with centers of the spinal cord, and it plays a role in reflexes that maintain posture. It appears red because it is richly supplied with blood vessels. The pons occupies the full thickness of the brainstem, but it is most visible ventrally as a rounded bulge where it separates the midbrain from the medulla oblongata. The ventral portion of the pons consists mostly of longitudinal nerve fibers, which relay information between the medulla oblongata and the cerebrum. Its ventral portion also contains large bundles of transverse nerve fibers"; "that wrap around to the back and connect with the cerebellum. They conduct impulses from the cerebrum to centers within the cerebellum. Several nuclei of the pons relay sensory information from peripheral nerves to higher brain centers. Other nuclei may function with centers of the medulla oblongata to control breathing. The medulla oblongata is an enlarged continuation of the spinal cord, extending from the level of the foramen magnum to the pons. Its dorsal surface flattens to form the floor of the fourth ventricle, and its ventral surface is marked by two longitudinal enlargements called the pyramids. These contain descending"; "separated by a layer of dura mater called the falx cerebelli. A structure called the vermis connects the cerebellar hemispheres at the midline. Like the cerebrum, the cerebellum is primarily composed of white matter with a thin layer of gray matter, the cerebellar cortex, on its surface. This cortex doubles over on itself in a series of complex folds that have myelinated nerve fibers branching into them. A cut into the cerebellum reveals a treelike pattern of white matter, called the arbor vitae, surrounded by gray matter. A number of nuclei lie deep within each cerebellar hemisphere. The largest and"; "REM sleep sometimes twitch their limbs. In humans, REM sleep usually lasts from five to fifteen minutes. This \"dream sleep\" is important. If a person lacks REM sleep for just one night, sleep on the next night makes up for it. During REM sleep, heart and respiratory rates are irregular. Marijuana, alcohol, and certain other drugs, such as benzodiazepines, interfere with REM sleep. it describes several disorders of sleep. The cerebellum is a large mass of tissue inferior to the occipital lobes of the cerebrum and dorsal to the pons and medulla oblongata. It consists of two lateral hemispheres partially" |]
    // getAnswerWithContext question [| "cerebrum Part of the brain in the upper part of the cranial cavity that provides higher mental functions." ; "cerebellum Part of the brain that coordinates skeletal muscle movement.";"The pattern of these elevations and depressions is complex, but, despite individual variations, is similar in all normal brains. For example, a longitudinal fissure separates the right and left cerebral hemispheres; a transverse fissure separates the cerebrum from the cerebellum; and sulci divide each hemisphere into lobes. Most of the five lobes of the cerebral hemispheres are named after the skull bones that they underlie. The lobes of each hemisphere include the following: All lobes of the cerebrum have a thin layer of gray matter called the cerebral cortex. The cortex constitutes the outermost portion of the cerebrum. It covers"; "matter called the red nucleus. This nucleus communicates with the cerebellum and with centers of the spinal cord, and it plays a role in reflexes that maintain posture. It appears red because it is richly supplied with blood vessels. The pons occupies the full thickness of the brainstem, but it is most visible ventrally as a rounded bulge where it separates the midbrain from the medulla oblongata. The ventral portion of the pons consists mostly of longitudinal nerve fibers, which relay information between the medulla oblongata and the cerebrum. Its ventral portion also contains large bundles of transverse nerve fibers"; "that wrap around to the back and connect with the cerebellum. They conduct impulses from the cerebrum to centers within the cerebellum. Several nuclei of the pons relay sensory information from peripheral nerves to higher brain centers. Other nuclei may function with centers of the medulla oblongata to control breathing. The medulla oblongata is an enlarged continuation of the spinal cord, extending from the level of the foramen magnum to the pons. Its dorsal surface flattens to form the floor of the fourth ventricle, and its ventral surface is marked by two longitudinal enlargements called the pyramids. These contain descending"; "separated by a layer of dura mater called the falx cerebelli. A structure called the vermis connects the cerebellar hemispheres at the midline. Like the cerebrum, the cerebellum is primarily composed of white matter with a thin layer of gray matter, the cerebellar cortex, on its surface. This cortex doubles over on itself in a series of complex folds that have myelinated nerve fibers branching into them. A cut into the cerebellum reveals a treelike pattern of white matter, called the arbor vitae, surrounded by gray matter. A number of nuclei lie deep within each cerebellar hemisphere. The largest and"; "REM sleep sometimes twitch their limbs. In humans, REM sleep usually lasts from five to fifteen minutes. This \"dream sleep\" is important. If a person lacks REM sleep for just one night, sleep on the next night makes up for it. During REM sleep, heart and respiratory rates are irregular. Marijuana, alcohol, and certain other drugs, such as benzodiazepines, interfere with REM sleep. it describes several disorders of sleep. The cerebellum is a large mass of tissue inferior to the occipital lobes of the cerebrum and dorsal to the pons and medulla oblongata. It consists of two lateral hemispheres partially" |]
    //DIFFERENCE/RELATIONSHIP, RELEVANCE FEEDBACK AS FIRST TWO DOCS, FOLLOWED BY GLOSSARY SENTENCE DOCS, FOLLOWED BY ELASTIC DOCS WITH BOTH TERMS IN THEM
    // getAnswerWithContext question [| "The cerebrum is the part of the brain responsible for voluntary muscle movement. The cerebellum is responsible for the coordination of the movement of the muscles in the opposite side of the cranial cavity. There is no direct connection between the two, but they are both parts of the same brain. It's a bit of a misnomer to think of them as two different things." ; "The cerebrum is the part of the brain responsible for voluntary movement. The cerebellum is responsible for involuntary movement. There is no difference between the two, they are just two different parts of the same brain. It's a bit like asking what is the difference between an arm and a leg. They are the same thing."; "The cerebellum is the part of the brain that coordinates skeletal muscle movement."; "The cerebrum is part of the brain in the upper part of the cranial cavity that provides higher mental functions." ; "The pattern of these elevations and depressions is complex, but, despite individual variations, is similar in all normal brains. For example, a longitudinal fissure separates the right and left cerebral hemispheres; a transverse fissure separates the cerebrum from the cerebellum; and sulci divide each hemisphere into lobes. Most of the five lobes of the cerebral hemispheres are named after the skull bones that they underlie. The lobes of each hemisphere include the following: All lobes of the cerebrum have a thin layer of gray matter called the cerebral cortex. The cortex constitutes the outermost portion of the cerebrum. It covers"; "matter called the red nucleus. This nucleus communicates with the cerebellum and with centers of the spinal cord, and it plays a role in reflexes that maintain posture. It appears red because it is richly supplied with blood vessels. The pons occupies the full thickness of the brainstem, but it is most visible ventrally as a rounded bulge where it separates the midbrain from the medulla oblongata. The ventral portion of the pons consists mostly of longitudinal nerve fibers, which relay information between the medulla oblongata and the cerebrum. Its ventral portion also contains large bundles of transverse nerve fibers"; "that wrap around to the back and connect with the cerebellum. They conduct impulses from the cerebrum to centers within the cerebellum. Several nuclei of the pons relay sensory information from peripheral nerves to higher brain centers. Other nuclei may function with centers of the medulla oblongata to control breathing. The medulla oblongata is an enlarged continuation of the spinal cord, extending from the level of the foramen magnum to the pons. Its dorsal surface flattens to form the floor of the fourth ventricle, and its ventral surface is marked by two longitudinal enlargements called the pyramids. These contain descending"; "separated by a layer of dura mater called the falx cerebelli. A structure called the vermis connects the cerebellar hemispheres at the midline. Like the cerebrum, the cerebellum is primarily composed of white matter with a thin layer of gray matter, the cerebellar cortex, on its surface. This cortex doubles over on itself in a series of complex folds that have myelinated nerve fibers branching into them. A cut into the cerebellum reveals a treelike pattern of white matter, called the arbor vitae, surrounded by gray matter. A number of nuclei lie deep within each cerebellar hemisphere. The largest and"; "REM sleep sometimes twitch their limbs. In humans, REM sleep usually lasts from five to fifteen minutes. This \"dream sleep\" is important. If a person lacks REM sleep for just one night, sleep on the next night makes up for it. During REM sleep, heart and respiratory rates are irregular. Marijuana, alcohol, and certain other drugs, such as benzodiazepines, interfere with REM sleep. it describes several disorders of sleep. The cerebellum is a large mass of tissue inferior to the occipital lobes of the cerebrum and dorsal to the pons and medulla oblongata. It consists of two lateral hemispheres partially" |]
    // USING CLOZE ITEM AS FIRST DOCUMENT, THEN GLOSSARY SENTENCES, THEN ELASTIC DOCS WITH BOTH TERMS IN THEM
    // getAnswerWithContext question [| "Small amounts enter the central canal of the spinal cord, but most CSF circulates through the subarachnoid space of both the brain and the spinal cord by passing through openings in the wall of the fourth ventricle near the cerebellum."; "The cerebrum is part of the brain in the upper part of the cranial cavity that provides higher mental functions." ; "The cerebellum is the part of the brain that coordinates skeletal muscle movement.";"The pattern of these elevations and depressions is complex, but, despite individual variations, is similar in all normal brains. For example, a longitudinal fissure separates the right and left cerebral hemispheres; a transverse fissure separates the cerebrum from the cerebellum; and sulci divide each hemisphere into lobes. Most of the five lobes of the cerebral hemispheres are named after the skull bones that they underlie. The lobes of each hemisphere include the following: All lobes of the cerebrum have a thin layer of gray matter called the cerebral cortex. The cortex constitutes the outermost portion of the cerebrum. It covers"; "matter called the red nucleus. This nucleus communicates with the cerebellum and with centers of the spinal cord, and it plays a role in reflexes that maintain posture. It appears red because it is richly supplied with blood vessels. The pons occupies the full thickness of the brainstem, but it is most visible ventrally as a rounded bulge where it separates the midbrain from the medulla oblongata. The ventral portion of the pons consists mostly of longitudinal nerve fibers, which relay information between the medulla oblongata and the cerebrum. Its ventral portion also contains large bundles of transverse nerve fibers"; "that wrap around to the back and connect with the cerebellum. They conduct impulses from the cerebrum to centers within the cerebellum. Several nuclei of the pons relay sensory information from peripheral nerves to higher brain centers. Other nuclei may function with centers of the medulla oblongata to control breathing. The medulla oblongata is an enlarged continuation of the spinal cord, extending from the level of the foramen magnum to the pons. Its dorsal surface flattens to form the floor of the fourth ventricle, and its ventral surface is marked by two longitudinal enlargements called the pyramids. These contain descending"; "separated by a layer of dura mater called the falx cerebelli. A structure called the vermis connects the cerebellar hemispheres at the midline. Like the cerebrum, the cerebellum is primarily composed of white matter with a thin layer of gray matter, the cerebellar cortex, on its surface. This cortex doubles over on itself in a series of complex folds that have myelinated nerve fibers branching into them. A cut into the cerebellum reveals a treelike pattern of white matter, called the arbor vitae, surrounded by gray matter. A number of nuclei lie deep within each cerebellar hemisphere. The largest and"; "REM sleep sometimes twitch their limbs. In humans, REM sleep usually lasts from five to fifteen minutes. This \"dream sleep\" is important. If a person lacks REM sleep for just one night, sleep on the next night makes up for it. During REM sleep, heart and respiratory rates are irregular. Marijuana, alcohol, and certain other drugs, such as benzodiazepines, interfere with REM sleep. it describes several disorders of sleep. The cerebellum is a large mass of tissue inferior to the occipital lobes of the cerebrum and dorsal to the pons and medulla oblongata. It consists of two lateral hemispheres partially" |]
    // USING GLOSSARY SENTENCES, THEN CLOZE ITEM AS FIRST DOCUMENT, THEN ELASTIC DOCS WITH BOTH TERMS IN THEM
    getAnswerWithContext question [| "The cerebrum is part of the brain in the upper part of the cranial cavity that provides higher mental functions." ; "The cerebellum is the part of the brain that coordinates skeletal muscle movement."; "Small amounts enter the central canal of the spinal cord, but most CSF circulates through the subarachnoid space of both the brain and the spinal cord by passing through openings in the wall of the fourth ventricle near the cerebellum.";"The pattern of these elevations and depressions is complex, but, despite individual variations, is similar in all normal brains. For example, a longitudinal fissure separates the right and left cerebral hemispheres; a transverse fissure separates the cerebrum from the cerebellum; and sulci divide each hemisphere into lobes. Most of the five lobes of the cerebral hemispheres are named after the skull bones that they underlie. The lobes of each hemisphere include the following: All lobes of the cerebrum have a thin layer of gray matter called the cerebral cortex. The cortex constitutes the outermost portion of the cerebrum. It covers"; "matter called the red nucleus. This nucleus communicates with the cerebellum and with centers of the spinal cord, and it plays a role in reflexes that maintain posture. It appears red because it is richly supplied with blood vessels. The pons occupies the full thickness of the brainstem, but it is most visible ventrally as a rounded bulge where it separates the midbrain from the medulla oblongata. The ventral portion of the pons consists mostly of longitudinal nerve fibers, which relay information between the medulla oblongata and the cerebrum. Its ventral portion also contains large bundles of transverse nerve fibers"; "that wrap around to the back and connect with the cerebellum. They conduct impulses from the cerebrum to centers within the cerebellum. Several nuclei of the pons relay sensory information from peripheral nerves to higher brain centers. Other nuclei may function with centers of the medulla oblongata to control breathing. The medulla oblongata is an enlarged continuation of the spinal cord, extending from the level of the foramen magnum to the pons. Its dorsal surface flattens to form the floor of the fourth ventricle, and its ventral surface is marked by two longitudinal enlargements called the pyramids. These contain descending"; "separated by a layer of dura mater called the falx cerebelli. A structure called the vermis connects the cerebellar hemispheres at the midline. Like the cerebrum, the cerebellum is primarily composed of white matter with a thin layer of gray matter, the cerebellar cortex, on its surface. This cortex doubles over on itself in a series of complex folds that have myelinated nerve fibers branching into them. A cut into the cerebellum reveals a treelike pattern of white matter, called the arbor vitae, surrounded by gray matter. A number of nuclei lie deep within each cerebellar hemisphere. The largest and"; "REM sleep sometimes twitch their limbs. In humans, REM sleep usually lasts from five to fifteen minutes. This \"dream sleep\" is important. If a person lacks REM sleep for just one night, sleep on the next night makes up for it. During REM sleep, heart and respiratory rates are irregular. Marijuana, alcohol, and certain other drugs, such as benzodiazepines, interfere with REM sleep. it describes several disorders of sleep. The cerebellum is a large mass of tissue inferior to the occipital lobes of the cerebrum and dorsal to the pons and medulla oblongata. It consists of two lateral hemispheres partially" |]
    // TWO KEY TERMS,  ONE DEFINITION, CLOZE, THEN ELASTIC DOCS WITH BOTH TERMS IN THEM
    // getAnswerWithContext question [| "A nerve is a bundle of axons in the peripheral nervous system."; "The pia mater is thin and contains many nerves , as well as blood vessels that nourish the underlying cells of the brain and spinal cord.";"lower four cervical nerves and the first thoracic nerve give rise to brachial plexuses. These networks of nerve fibers are deep in the shoulders between the neck and the axillae. The major branches emerging from the brachial plexuses include the following : Other nerves associated with the brachial plexus that innervate various skeletal muscles include the following: The ventral branches of the lumbar and first four sacral nerves form the lumbosacral plexuses. The lumbar portions are in the lumbar regions of the abdomen and the sacral portions are in the pelvic cavity. These networks of nerve fibers give rise to";"the brain or spinal cord, are called sensory nerves. Nerves that have only fibers involved in motor control are motor nerves. Most nerves include both sensory and motor fibers and are called mixed nerves. Nerves originating from the brain that communicate with other body parts are called cranial nerves, whereas nerves originating from the spinal cord that communicate with other body parts are called spinal nerves. The nerve fibers in the cranial and spinal nerves can be subdivided further into four groups as follows: The term general in each of these categories indicates that the fibers are associated with general";"Fibers in the motor component of the glossopharyngeal nerves innervate certain salivary glands and a constrictor muscle in the wall of the pharynx that functions in swallowing. The tenth pair of cranial nerves, the vagus nerves, originate in the medulla oblongata and extend downward through the neck into the chest and abdomen. These nerves are mixed, including both somatic and autonomic branches, with the autonomic fibers predominant. Among the somatic components of the vagus nerves are motor fibers that conduct impulses to muscles of the larynx and pharynx. These fibers are associated with speech and swallowing reflexes that use muscles"; "structures such as the skin, skeletal muscles, glands, and viscera. Three other groups of fibers, found only in cranial nerves, are associated with more specialized, or special, structures: Twelve pairs of cranial nerves are located on the underside of the brain. Most of the cranial nerves are mixed nerves, but some of those associated with special senses, such as smell and vision, have only sensory fibers. Other cranial nerves that innervate muscles and glands are primarily composed of motor fibers. The first pair, which is sensory, has fibers that begin in the nasal cavity and synapse in the frontal lobe";"recombined in a way that enables fibers associated with a particular peripheral body part to reach it in the same peripheral nerve, even though the fibers originate from different spinal nerves. The anterior branches of the first four cervical nerves form the cervical plexuses, which lie deep in the neck on either side. Fibers from these plexuses supply the muscles and skin of the neck. In addition, fibers from the third, fourth, and fifth cervical nerves pass into the right and left phrenic nerves, which conduct motor impulses to the muscle fibers of the diaphragm. The anterior branches of the"; "the soft palate, pharynx, and larynx. The spinal branch descends into the neck and supplies motor fibers to the trapezius and sternocleidomastoid muscles. This nerve is considered motor, with some proprioceptive fibers. The twelfth pair of cranial nerves, the hypoglossal nerves, arise from the medulla oblongata and pass into the tongue. They primarily consist of fibers that conduct impulses to muscles that move the tongue in speaking, chewing, and swallowing. This nerve is considered motor, with some proprioceptive fibers. it summarizes the functions of the cranial nerves. Thirty-one pairs of spinal nerves originate from the spinal cord. The first pair";"of contraction of skeletal muscles. However, because these proprioceptive fibers contribute directly to motor control, cranial nerves whose only sensory component is from such proprioceptors are usually considered motor nerves. This pertains to cranial nerves III, IV, VI, XI, and XII. Neuron cell bodies from which the sensory fibers in the cranial nerves arise are outside the brain, and most are in groups called ganglia. However, most motor neuron cell bodies are in the gray matter of the brain. Numbers and names designate cranial nerves. The numbers indicate the order in which the nerves arise from the brain, from anterior"; "muscles, which are not supplied by the oculomotor nerves. The trochlear nerve is considered motor, with some proprioceptive fibers. The fifth pair of cranial nerves, the trigeminal nerves, are the largest and arise from the pons. They are mixed nerves, with more extensive sensory portions. Each sensory component includes three large branches, called the ophthalmic, maxillary, and mandibular divisions. The ophthalmic division of each the trigeminal nerve consists of sensory fibers that conduct impulses to the brain from the surface of the eye; the tear gland; and the skin of the anterior scalp, forehead, and upper eyelid. The fibers of"; "They enter the orbits of the eyes and supply motor impulses to the remaining pair of external eye muscles, the lateral rectus muscles. This nerve is considered motor, with some proprioceptive fibers. The seventh pair of cranial nerves, the facial nerves, are mixed nerves that arise from the lower part of the pons and emerge on the sides of the face. Their sensory branches are associated with taste receptors on the anterior two-thirds of the tongue, and some of their motor fibers conduct impulses to muscles of facial expression. Still other motor fibers of these nerves function in the autonomic"|]
    // TWO KEY TERMS, TWO DEFINITIONS,  CLOZE, THEN ELASTIC DOCS WITH BOTH TERMS IN THEM
    // getAnswerWithContext question [| "A nerve fiber is an axon." ; "A nerve fiber conducts an impulse away from a neuron cell body." ; "A nerve is a bundle of axons in the peripheral nervous system."; "The pia mater is thin and contains many nerves , as well as blood vessels that nourish the underlying cells of the brain and spinal cord.";"lower four cervical nerves and the first thoracic nerve give rise to brachial plexuses. These networks of nerve fibers are deep in the shoulders between the neck and the axillae. The major branches emerging from the brachial plexuses include the following : Other nerves associated with the brachial plexus that innervate various skeletal muscles include the following: The ventral branches of the lumbar and first four sacral nerves form the lumbosacral plexuses. The lumbar portions are in the lumbar regions of the abdomen and the sacral portions are in the pelvic cavity. These networks of nerve fibers give rise to";"the brain or spinal cord, are called sensory nerves. Nerves that have only fibers involved in motor control are motor nerves. Most nerves include both sensory and motor fibers and are called mixed nerves. Nerves originating from the brain that communicate with other body parts are called cranial nerves, whereas nerves originating from the spinal cord that communicate with other body parts are called spinal nerves. The nerve fibers in the cranial and spinal nerves can be subdivided further into four groups as follows: The term general in each of these categories indicates that the fibers are associated with general";"Fibers in the motor component of the glossopharyngeal nerves innervate certain salivary glands and a constrictor muscle in the wall of the pharynx that functions in swallowing. The tenth pair of cranial nerves, the vagus nerves, originate in the medulla oblongata and extend downward through the neck into the chest and abdomen. These nerves are mixed, including both somatic and autonomic branches, with the autonomic fibers predominant. Among the somatic components of the vagus nerves are motor fibers that conduct impulses to muscles of the larynx and pharynx. These fibers are associated with speech and swallowing reflexes that use muscles"; "structures such as the skin, skeletal muscles, glands, and viscera. Three other groups of fibers, found only in cranial nerves, are associated with more specialized, or special, structures: Twelve pairs of cranial nerves are located on the underside of the brain. Most of the cranial nerves are mixed nerves, but some of those associated with special senses, such as smell and vision, have only sensory fibers. Other cranial nerves that innervate muscles and glands are primarily composed of motor fibers. The first pair, which is sensory, has fibers that begin in the nasal cavity and synapse in the frontal lobe";"recombined in a way that enables fibers associated with a particular peripheral body part to reach it in the same peripheral nerve, even though the fibers originate from different spinal nerves. The anterior branches of the first four cervical nerves form the cervical plexuses, which lie deep in the neck on either side. Fibers from these plexuses supply the muscles and skin of the neck. In addition, fibers from the third, fourth, and fifth cervical nerves pass into the right and left phrenic nerves, which conduct motor impulses to the muscle fibers of the diaphragm. The anterior branches of the"; "the soft palate, pharynx, and larynx. The spinal branch descends into the neck and supplies motor fibers to the trapezius and sternocleidomastoid muscles. This nerve is considered motor, with some proprioceptive fibers. The twelfth pair of cranial nerves, the hypoglossal nerves, arise from the medulla oblongata and pass into the tongue. They primarily consist of fibers that conduct impulses to muscles that move the tongue in speaking, chewing, and swallowing. This nerve is considered motor, with some proprioceptive fibers. it summarizes the functions of the cranial nerves. Thirty-one pairs of spinal nerves originate from the spinal cord. The first pair";"of contraction of skeletal muscles. However, because these proprioceptive fibers contribute directly to motor control, cranial nerves whose only sensory component is from such proprioceptors are usually considered motor nerves. This pertains to cranial nerves III, IV, VI, XI, and XII. Neuron cell bodies from which the sensory fibers in the cranial nerves arise are outside the brain, and most are in groups called ganglia. However, most motor neuron cell bodies are in the gray matter of the brain. Numbers and names designate cranial nerves. The numbers indicate the order in which the nerves arise from the brain, from anterior"; "muscles, which are not supplied by the oculomotor nerves. The trochlear nerve is considered motor, with some proprioceptive fibers. The fifth pair of cranial nerves, the trigeminal nerves, are the largest and arise from the pons. They are mixed nerves, with more extensive sensory portions. Each sensory component includes three large branches, called the ophthalmic, maxillary, and mandibular divisions. The ophthalmic division of each the trigeminal nerve consists of sensory fibers that conduct impulses to the brain from the surface of the eye; the tear gland; and the skin of the anterior scalp, forehead, and upper eyelid. The fibers of"; "They enter the orbits of the eyes and supply motor impulses to the remaining pair of external eye muscles, the lateral rectus muscles. This nerve is considered motor, with some proprioceptive fibers. The seventh pair of cranial nerves, the facial nerves, are mixed nerves that arise from the lower part of the pons and emerge on the sides of the face. Their sensory branches are associated with taste receptors on the anterior two-thirds of the tongue, and some of their motor fibers conduct impulses to muscles of facial expression. Still other motor fibers of these nerves function in the autonomic"|]

type Tag =
    /// Were definitions inserted as context documents in the order of words in question
    | DefinitionsUsed of int
    /// Was the cloze sentence used as a context document
    | ClozeUsed of bool
    /// Number of Elasticsearch documents returned
    | ElasticDocumentsFound of int
    /// Number of Elasticsearch documents with both answers
    | ElasticDocumentsContainBothKeys of int
    /// Number of Elasticsearch documents used (for whatever reason)
    | ElasticDocumentsUsed of int
    /// Number of answer sentences with one key term
    | AnswerSentencesContainOneKey of int
    /// Was coreference used to clean up answer
    | CoreferenceFilteredSentences of bool
    /// Was the synthetic question "What is the difference between X and Y"
    | DifferenceQuestion
    /// Was the synthetic question "What is the relationship between X and Y"
    | RelationshipQuestion
    /// Debug information
    | Trace of string

/// An elaborated feedback based on the longform-qa API
type ElaboratedFeedback =
    {
        /// The output elaborated feedback as a string
        ElaboratedFeedback : string
        /// The sentence that the cloze item was generated from, i.e. without a blank
        ClozeSentence : string;
        /// The incorrect answer to the cloze item provided by the student
        IncorrectAnswer : string
        /// The definition we retrieved for the IncorrectAnswer
        IncorrectAnswerDefinition : string option
        /// The correct answer to the cloze item
        CorrectAnswer : string
        /// The definition we retrieved for the CorrectAnswer
        CorrectAnswerDefinition : string option
        /// Documents retrieved from Elasticsearch AP book using the synthetic question
        ElasticDocuments : Document[]
        /// Index of ElasticDocuments that contain both IncorrectAnswer and CorrectAnswer
        MatchingElasticDocuments : int[]
        /// Documents provided as longform QA context
        ContextDocuments : string[]
        /// The unprocessed answer sentences, may contain garbage sentences
        CandidateAnswerSentences : string[];
        /// Coref resolved candidate answer sentences
        CorefCandidateAnswerSentences : string[];
        /// Index of CorefCandidateAnswer sentences used
        SelectedSentences : int[];
        /// The synthethic question used, e.g What is the difference between X and Y?
        SyntheticQuestion : string;
        /// List of tags for analysis and debugging
        Tags : Tag[]
    }

/// An entailment comparison between elaborated feedbacks
type EntailmentComparison =
    {
        Premise : ElaboratedFeedback
        Hypothesis : ElaboratedFeedback
        Entailment : float
        Contradiction : float
        Neutral : float
    }

let lower ( s : string ) = s.ToLower()
let lowContains ( a : string ) ( b : string ) =
    a.ToLower().Contains( b.ToLower () )


let GetElaboratedFeedback( incorrectAnswer : string ) (correctAnswer : string) ( clozeSentence : string ) =
    promise {
        let tags = ResizeArray<Tag>()
        //Get definitions
        let incorrectDefinition = incorrectAnswer |> DefinitionalFeedback.GetDefinitionFromGlossary
        let correctDefinition = correctAnswer |> DefinitionalFeedback.GetDefinitionFromGlossary
        let definitions = [| incorrectDefinition ; correctDefinition |] |> Array.choose id;
        tags.Add(DefinitionsUsed(definitions.Length))
        
        //Create synthetic question
        let question = "What is the relationship between the " + incorrectAnswer + " and the " + correctAnswer + "?"
        tags.Add(RelationshipQuestion)

        //Get elastic docs; see tutorial dialogue for example of handling promises if things get more complicated
        let! documents = question |> getDocuments
        match documents with
        | Ok(docs) ->
            tags.Add(ElasticDocumentsFound(docs.Length))
            //Remove documents that don't contain both incorrect and correct answers
            let filteredDocuments = docs |> Array.skip 1 |> Array.mapi( fun i d -> i,d ) |> Array.filter( fun (_,d) ->  (lowContains d.Text incorrectAnswer) && (lowContains d.Text correctAnswer) )
            tags.Add(ElasticDocumentsContainBothKeys(filteredDocuments.Length))
            let retainedDocIndices = filteredDocuments |> Array.map fst

            //Heuristic limit on number of elastic documents to use (idea is that too much irrelevant context is harmful)
            let elasticDocLimit = 3
            let tryTake i (arr:string[]) = if i > arr.Length then arr else arr.[0..i-1]
            let finalElasticDocs = filteredDocuments |> Array.map( fun (_,d) -> d.Text) |> tryTake elasticDocLimit
            tags.Add(ElasticDocumentsUsed(finalElasticDocs.Length))

            //Get an answer with custom context
            let context = Array.concat [ definitions; [| clozeSentence |]; finalElasticDocs ]
            //DEBUG
            // let context = Array.concat [
            //         [|
            //         "The cerebrum is part of the brain in the upper part of the cranial cavity that provides higher mental functions.";
            //         "The cerebellum is the part of the brain that coordinates skeletal muscle movement.";
            //         "Small amounts enter the central canal of the spinal cord, but most CSF circulates through the subarachnoid space of both the brain and the spinal cord by passing through openings in the wall of the fourth ventricle near the cerebellum."
            //         |];
            //         (filteredDocuments |> Array.map( fun (_,d) -> d.Text) |> Array.take 5 )
            //     ]
            tags.Add(ClozeUsed(true))

            let! answer = getAnswerWithContext question context
            match answer with
            | Ok(ans) -> 
                //Remove sentences that do not contain one of our terms; we get better recall by using coref resolution to do this
                let! answerWithResolvedReferents = ans.answer |> AllenNLP.ResolveTextReferents
                match answerWithResolvedReferents with
                | Ok(res) ->
                    // let sentences = ans.answer.Split('.') //hacking sentence segmentation
                    //align before filtering
                    let sentenceTuples =  res.documentAnnotation.sentences |> Array.zip res.resolvedSentences |> Array.mapi( fun i (r,s) -> i, r, s.sen)
                    let candidateSentences = sentenceTuples |> Array.map( fun (i,r,s) -> s)
                    //filter
                    let filteredTuples = sentenceTuples |> Array.filter( fun (i,r,s) ->  (lowContains r incorrectAnswer) || (lowContains r correctAnswer) )
                    //project results
                    let corefSentences = filteredTuples |> Array.map( fun (i,r,s) -> r)
                    let answerSentences = filteredTuples |> Array.map( fun (i,r,s) -> s)
                    let retainedSentenceIndices = filteredTuples |> Array.map( fun (i,r,s) -> i)
                    tags.Add(AnswerSentencesContainOneKey(answerSentences.Length))
                    tags.Add(CoreferenceFilteredSentences(true))

                    let finalAnswer = answerSentences |> String.concat " "
                    let elaboratedFeedback = 
                        {
                            IncorrectAnswer = incorrectAnswer;
                            IncorrectAnswerDefinition = incorrectDefinition;
                            CorrectAnswer = correctAnswer;
                            CorrectAnswerDefinition = correctDefinition;
                            ElasticDocuments = docs;
                            MatchingElasticDocuments = retainedDocIndices;
                            ContextDocuments = context;
                            ClozeSentence = clozeSentence;
                            CandidateAnswerSentences = candidateSentences;
                            CorefCandidateAnswerSentences = corefSentences;
                            SelectedSentences = retainedSentenceIndices;
                            ElaboratedFeedback = finalAnswer;
                            SyntheticQuestion = question;
                            Tags = tags.ToArray()
                        }
                    return Ok( elaboratedFeedback ) 
                // Error on resolving coreference
                | Error(e) -> 
                    return Error( FetchError.DecodingFailed(e) ) //HACK: must be a fetch error to match type, so we choose a FetchError that wraps string
            // Error on getting answer from qa
            | Error(e) -> 
                return Error(e)
        // Error on getting docs from elasticsearch
        | Error(e) -> 
            return Error(e)
    }

/// Request for the test harness UI
type HarnessElaboratedFeedbackRequest =
    { 
        CorrectAnswer : string
        IncorrectAnswer : string
        ClozeSentence : string
    }
    static member InitializeTest() = {CorrectAnswer="cerebellum"; IncorrectAnswer ="cerebrum"; ClozeSentence="Small amounts enter the central canal of the spinal cord, but most CSF circulates through the subarachnoid space of both the brain and the spinal cord by passing through openings in the wall of the fourth ventricle near the cerebellum ."}
    
/// This function should only be called by the test harness GUI. It wraps GenerateFeedback to match the test harness API
let HarnessGetElaboratedFeedback jsonRequest =
    let request = jsonRequest |> ofJson<HarnessElaboratedFeedbackRequest>
    GetElaboratedFeedback request.IncorrectAnswer request.CorrectAnswer request.ClozeSentence