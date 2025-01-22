package org.apache.tools.ant.taskdefs.optional;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.XMLValidateTask.Attribute;
import org.apache.tools.ant.taskdefs.optional.XMLValidateTask.Property;
import org.apache.tools.ant.taskdefs.optional.XMLValidateTask.ValidatorErrorHandler;
import org.apache.tools.ant.types.DTDLocation;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.XMLCatalog;
import org.junit.Test;
import org.xml.sax.EntityResolver;
import org.xml.sax.SAXParseException;

public class XMLValidateTaskDiffblueTest {
  /**
   * Test Attribute getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Attribute}
   *   <li>{@link Attribute#setName(String)}
   *   <li>{@link Attribute#setValue(boolean)}
   *   <li>{@link Attribute#getName()}
   *   <li>{@link Attribute#getValue()}
   * </ul>
   */
  @Test
  public void testAttributeGettersAndSetters() {
    // Arrange and Act
    Attribute actualAttribute = new Attribute();
    actualAttribute.setName("Name");
    actualAttribute.setValue(true);
    String actualName = actualAttribute.getName();

    // Assert
    assertEquals("Name", actualName);
    assertTrue(actualAttribute.getValue());
  }

  /**
   * Test Property getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Property}
   *   <li>{@link Property#setName(String)}
   *   <li>{@link Property#setValue(String)}
   *   <li>{@link Property#getName()}
   *   <li>{@link Property#getValue()}
   * </ul>
   */
  @Test
  public void testPropertyGettersAndSetters() {
    // Arrange and Act
    Property actualProperty = new Property();
    actualProperty.setName("Name");
    actualProperty.setValue("42");
    String actualName = actualProperty.getName();

    // Assert
    assertEquals("42", actualProperty.getValue());
    assertEquals("Name", actualName);
  }

  /**
   * Test {@link XMLValidateTask#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath} Project is {@code null}.</li>
   *   <li>When {@link Path#Path(Project)} with project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenSystemBootClasspathProjectIsNull_whenPathWithProjectIsNull() {
    // Arrange
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClasspath(classpath);

    // Act
    xmlValidateTask.setClasspath(new Path(null));

    // Assert that nothing has changed
    assertFalse(xmlValidateTask.classpath.isReference());
    assertTrue(xmlValidateTask.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link XMLValidateTask} (default constructor) {@link XMLValidateTask#classpath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenXMLValidateTaskClasspathProjectIsProject() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    Project project = new Project();
    xmlValidateTask.setClasspath(new Path(project));

    // Act
    xmlValidateTask.setClasspath(Path.systemBootClasspath);

    // Assert that nothing has changed
    assertSame(project, xmlValidateTask.classpath.getProject());
  }

  /**
   * Test {@link XMLValidateTask#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then not {@link XMLValidateTask} (default constructor) {@link XMLValidateTask#classpath} Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenNotXMLValidateTaskClasspathReference() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClasspath(Path.systemBootClasspath);

    // Act
    xmlValidateTask.setClasspath(null);

    // Assert that nothing has changed
    assertFalse(xmlValidateTask.classpath.isReference());
    assertTrue(xmlValidateTask.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#createClasspath()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor).</li>
   *   <li>Then return Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenXMLValidateTask_thenReturnProjectIsNull() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    // Act
    Path actualCreateClasspathResult = xmlValidateTask.createClasspath();

    // Assert
    Path path = xmlValidateTask.classpath;
    assertNull(path.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(path.getProject());
    assertNull(path.getRefid());
    assertEquals(0, path.size());
    assertFalse(path.isReference());
    assertTrue(path.isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link XMLValidateTask} (default constructor) {@link XMLValidateTask#classpath} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenXMLValidateTaskClasspathProjectIsNull() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    // Act
    xmlValidateTask.setClasspathRef(new Reference("42"));

    // Assert
    Path path = xmlValidateTask.classpath;
    assertNull(path.getDescription());
    assertNull(path.getProject());
    assertNull(path.getRefid());
    assertFalse(path.isReference());
  }

  /**
   * Test {@link XMLValidateTask#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link XMLValidateTask} (default constructor) {@link XMLValidateTask#classpath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenXMLValidateTaskClasspathProjectIsProject() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    Project project = new Project();
    xmlValidateTask.setProject(project);

    // Act
    xmlValidateTask.setClasspathRef(new Reference("42"));

    // Assert
    Path path = xmlValidateTask.classpath;
    assertNull(path.getDescription());
    assertNull(path.getRefid());
    assertFalse(path.isReference());
    assertSame(project, path.getProject());
  }

  /**
   * Test {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}.
   * <p>
   * Method under test: {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlValidateTask.addConfiguredXMLCatalog(new XMLCatalog());

    // Assert that nothing has changed
    EntityResolver entityResolver = xmlValidateTask.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link XMLCatalog} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenProject_whenXMLCatalogProjectIsProject() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    XMLCatalog catalog = new XMLCatalog();
    catalog.setProject(new Project());
    catalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlValidateTask.addConfiguredXMLCatalog(catalog);

    // Assert
    EntityResolver entityResolver = xmlValidateTask.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenXMLCatalog() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    XMLCatalog catalog = new XMLCatalog();
    catalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlValidateTask.addConfiguredXMLCatalog(catalog);

    // Assert
    EntityResolver entityResolver = xmlValidateTask.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>When {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_whenXMLCatalog() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    // Act
    xmlValidateTask.addConfiguredXMLCatalog(new XMLCatalog());

    // Assert
    EntityResolver entityResolver = xmlValidateTask.getEntityResolver();
    assertTrue(entityResolver instanceof XMLCatalog);
    Path catalogPath = ((XMLCatalog) entityResolver).getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XMLValidateTask#addFileset(FileSet)}.
   * <p>
   * Method under test: {@link XMLValidateTask#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    FileSet set = new FileSet();

    // Act
    xmlValidateTask.addFileset(set);

    // Assert
    Vector<FileSet> fileSetList = xmlValidateTask.filesets;
    assertEquals(1, fileSetList.size());
    assertSame(set, fileSetList.get(0));
  }

  /**
   * Test {@link XMLValidateTask#createAttribute()}.
   * <p>
   * Method under test: {@link XMLValidateTask#createAttribute()}
   */
  @Test
  public void testCreateAttribute() {
    // Arrange and Act
    Attribute actualCreateAttributeResult = (new XMLValidateTask()).createAttribute();

    // Assert
    assertNull(actualCreateAttributeResult.getName());
    assertFalse(actualCreateAttributeResult.getValue());
  }

  /**
   * Test {@link XMLValidateTask#createProperty()}.
   * <p>
   * Method under test: {@link XMLValidateTask#createProperty()}
   */
  @Test
  public void testCreateProperty() {
    // Arrange and Act
    Property actualCreatePropertyResult = (new XMLValidateTask()).createProperty();

    // Assert
    assertNull(actualCreatePropertyResult.getName());
    assertNull(actualCreatePropertyResult.getValue());
  }

  /**
   * Test {@link XMLValidateTask#createDTD()}.
   * <p>
   * Method under test: {@link XMLValidateTask#createDTD()}
   */
  @Test
  public void testCreateDTD() {
    // Arrange and Act
    DTDLocation actualCreateDTDResult = (new XMLValidateTask()).createDTD();

    // Assert
    assertNull(actualCreateDTDResult.getLocation());
    assertNull(actualCreateDTDResult.getPublicId());
    assertNull(actualCreateDTDResult.getBase());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link XMLValidateTask#setClassName(String)}
   *   <li>{@link XMLValidateTask#setFailOnError(boolean)}
   *   <li>{@link XMLValidateTask#setFile(File)}
   *   <li>{@link XMLValidateTask#setLenient(boolean)}
   *   <li>{@link XMLValidateTask#setWarn(boolean)}
   *   <li>{@link XMLValidateTask#getEntityResolver()}
   *   <li>{@link XMLValidateTask#getXmlReader()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    // Act
    xmlValidateTask.setClassName("Class Name");
    xmlValidateTask.setFailOnError(true);
    xmlValidateTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    xmlValidateTask.setLenient(true);
    xmlValidateTask.setWarn(true);
    EntityResolver actualEntityResolver = xmlValidateTask.getEntityResolver();

    // Assert
    assertTrue(actualEntityResolver instanceof XMLCatalog);
    assertNull(xmlValidateTask.getXmlReader());
  }

  /**
   * Test {@link XMLValidateTask#execute()}.
   * <p>
   * Method under test: {@link XMLValidateTask#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    xmlValidateTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.execute());
  }

  /**
   * Test {@link XMLValidateTask#execute()}.
   * <p>
   * Method under test: {@link XMLValidateTask#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "File ").toFile());
    xmlValidateTask.addFileset(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.execute());
  }

  /**
   * Test {@link XMLValidateTask#execute()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#execute()}
   */
  @Test
  public void testExecute_givenXMLValidateTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XMLValidateTask()).execute());
  }

  /**
   * Test {@link XMLValidateTask#initValidator()}.
   * <p>
   * Method under test: {@link XMLValidateTask#initValidator()}
   */
  @Test
  public void testInitValidator() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("org.apache.tools.ant.taskdefs.optional.XMLValidateTask$Attribute");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.initValidator());
  }

  /**
   * Test {@link XMLValidateTask#initValidator()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) ClassName is {@code org.xml.sax.Parser}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#initValidator()}
   */
  @Test
  public void testInitValidator_givenXMLValidateTaskClassNameIsOrgXmlSaxParser() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("org.xml.sax.Parser");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.initValidator());
  }

  /**
   * Test {@link XMLValidateTask#initValidator()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) ClassName is {@code Using SAX2 reader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#initValidator()}
   */
  @Test
  public void testInitValidator_givenXMLValidateTaskClassNameIsUsingSax2Reader() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("Using SAX2 reader ");

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.initValidator());
  }

  /**
   * Test {@link XMLValidateTask#isSax1Parser()}.
   * <p>
   * Method under test: {@link XMLValidateTask#isSax1Parser()}
   */
  @Test
  public void testIsSax1Parser() {
    // Arrange, Act and Assert
    assertFalse((new XMLValidateTask()).isSax1Parser());
  }

  /**
   * Test {@link XMLValidateTask#createXmlReader()}.
   * <p>
   * Method under test: {@link XMLValidateTask#createXmlReader()}
   */
  @Test
  public void testCreateXmlReader() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("org.apache.tools.ant.taskdefs.optional.XMLValidateTask$Attribute");
    xmlValidateTask.setClasspath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.createXmlReader());
  }

  /**
   * Test {@link XMLValidateTask#createXmlReader()}.
   * <p>
   * Method under test: {@link XMLValidateTask#createXmlReader()}
   */
  @Test
  public void testCreateXmlReader2() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(new Project());
    xmlValidateTask.setClassName("foo");
    xmlValidateTask.setClasspath(new Path(new Project(), "Could not start xml validation: "));

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.createXmlReader());
  }

  /**
   * Test {@link XMLValidateTask#createXmlReader()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) ClassName is {@code org.xml.sax.Parser}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#createXmlReader()}
   */
  @Test
  public void testCreateXmlReader_givenXMLValidateTaskClassNameIsOrgXmlSaxParser() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("org.xml.sax.Parser");
    xmlValidateTask.setClasspath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.createXmlReader());
  }

  /**
   * Test {@link XMLValidateTask#createXmlReader()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) Classpath is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#createXmlReader()}
   */
  @Test
  public void testCreateXmlReader_givenXMLValidateTaskClasspathIsNull_thenThrowBuildException() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("foo");
    xmlValidateTask.setClasspath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.createXmlReader());
  }

  /**
   * Test {@link XMLValidateTask#createXmlReader()}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) Classpath is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#createXmlReader()}
   */
  @Test
  public void testCreateXmlReader_givenXMLValidateTaskClasspathIsPathWithProjectIsProject() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(new Project());
    xmlValidateTask.setClassName("foo");
    xmlValidateTask.setClasspath(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask.createXmlReader());
  }

  /**
   * Test {@link XMLValidateTask#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenName_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XMLValidateTask()).setProperty("Name", null));
  }

  /**
   * Test {@link XMLValidateTask#setProperty(String, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#setProperty(String, String)}
   */
  @Test
  public void testSetProperty_whenNull_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XMLValidateTask()).setProperty(null, null));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("org.apache.tools.ant.taskdefs.optional.XMLValidateTask$Attribute");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenProjectAddBuildListenerDefaultLogger_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) ClassName is {@code org.xml.sax.Parser}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenXMLValidateTaskClassNameIsOrgXmlSaxParser() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("org.xml.sax.Parser");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) ClassName is {@code Using SAX2 reader}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenXMLValidateTaskClassNameIsUsingSax2Reader() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setClassName("Using SAX2 reader ");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) Lenient is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenXMLValidateTaskLenientIsTrue_thenThrowBuildException() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setLenient(true);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenXMLValidateTaskProjectIsProject_thenThrowBuildException() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_givenXMLValidateTask_thenThrowBuildException() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> xmlValidateTask.doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link XMLValidateTask#doValidate(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code Using SAX2 reader} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLValidateTask#doValidate(File)}
   */
  @Test
  public void testDoValidate_whenPropertyIsJavaIoTmpdirIsUsingSax2ReaderToFile() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();

    // Act and Assert
    assertThrows(BuildException.class, () -> xmlValidateTask
        .doValidate(Paths.get(System.getProperty("java.io.tmpdir"), "Using SAX2 reader ").toFile()));
  }

  /**
   * Test new {@link XMLValidateTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XMLValidateTask}
   */
  @Test
  public void testNewXMLValidateTask() {
    // Arrange and Act
    XMLValidateTask actualXmlValidateTask = new XMLValidateTask();

    // Assert
    assertTrue(actualXmlValidateTask.getEntityResolver() instanceof XMLCatalog);
    assertNull(actualXmlValidateTask.file);
    assertNull(actualXmlValidateTask.getDescription());
    assertNull(actualXmlValidateTask.getTaskName());
    assertNull(actualXmlValidateTask.getTaskType());
    assertNull(actualXmlValidateTask.readerClassName);
    assertNull(actualXmlValidateTask.getProject());
    assertNull(actualXmlValidateTask.getOwningTarget());
    assertNull(actualXmlValidateTask.classpath);
    assertNull(actualXmlValidateTask.getXmlReader());
    assertFalse(actualXmlValidateTask.isSax1Parser());
    assertFalse(actualXmlValidateTask.lenient);
    assertTrue(actualXmlValidateTask.filesets.isEmpty());
    assertTrue(actualXmlValidateTask.failOnError);
    assertTrue(actualXmlValidateTask.warn);
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError2() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "file:", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError3() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", null, 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError4() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "foo", -1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError5() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "foo", 1, -1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError6() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new SchemaValidate()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "file:", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(":", typeClass);
    project.addBuildListener(new AntClassLoader());

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(project);
    ValidatorErrorHandler validatorErrorHandler = xmlValidateTask.new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(project);
    ValidatorErrorHandler validatorErrorHandler = xmlValidateTask.new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#error(SAXParseException)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ValidatorErrorHandler#error(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerError_givenXMLValidateTaskProjectIsProject() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(new Project());
    ValidatorErrorHandler validatorErrorHandler = xmlValidateTask.new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.error(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError2() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "file:", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError3() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", null, 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError4() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "foo", -1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError5() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "foo", 1, -1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(":", typeClass);
    project.addBuildListener(new AntClassLoader());

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(project);
    ValidatorErrorHandler validatorErrorHandler = xmlValidateTask.new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(project);
    ValidatorErrorHandler validatorErrorHandler = xmlValidateTask.new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#fatalError(SAXParseException)}.
   * <ul>
   *   <li>Given {@link XMLValidateTask} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ValidatorErrorHandler#fatalError(SAXParseException)}
   */
  @Test
  public void testValidatorErrorHandlerFatalError_givenXMLValidateTaskProjectIsProject() {
    // Arrange
    XMLValidateTask xmlValidateTask = new XMLValidateTask();
    xmlValidateTask.setProject(new Project());
    ValidatorErrorHandler validatorErrorHandler = xmlValidateTask.new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.fatalError(new SAXParseException("foo", "foo", "foo", 1, 1));

    // Assert
    assertTrue(validatorErrorHandler.getFailure());
  }

  /**
   * Test ValidatorErrorHandler getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ValidatorErrorHandler#ValidatorErrorHandler(XMLValidateTask)}
   *   <li>{@link ValidatorErrorHandler#getFailure()}
   * </ul>
   */
  @Test
  public void testValidatorErrorHandlerGettersAndSetters() {
    // Arrange, Act and Assert
    assertFalse(((new XMLValidateTask()).new ValidatorErrorHandler()).getFailure());
  }

  /**
   * Test ValidatorErrorHandler {@link ValidatorErrorHandler#init(File)}.
   * <p>
   * Method under test: {@link ValidatorErrorHandler#init(File)}
   */
  @Test
  public void testValidatorErrorHandlerInit() {
    // Arrange
    ValidatorErrorHandler validatorErrorHandler = (new XMLValidateTask()).new ValidatorErrorHandler();

    // Act
    validatorErrorHandler.init(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    File file = validatorErrorHandler.currentFile;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
  }
}
