package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.OutputStream;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.XSLTProcess.Factory;
import org.apache.tools.ant.taskdefs.XSLTProcess.Factory.Attribute;
import org.apache.tools.ant.taskdefs.XSLTProcess.Factory.Feature;
import org.apache.tools.ant.taskdefs.XSLTProcess.OutputProperty;
import org.apache.tools.ant.taskdefs.XSLTProcess.Param;
import org.apache.tools.ant.taskdefs.XSLTProcess.TraceConfiguration;
import org.apache.tools.ant.taskdefs.optional.TraXLiaison;
import org.apache.tools.ant.types.Environment;
import org.apache.tools.ant.types.Environment.Variable;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Mapper;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.PolyTest;
import org.apache.tools.ant.types.PolyTest.MyPath;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.XMLCatalog;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.Resources;
import org.apache.tools.ant.util.FileNameMapper;
import org.junit.Test;

public class XSLTProcessDiffblueTest {
  /**
   * Test {@link XSLTProcess#addMapper(Mapper)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addMapper(Mapper)}
   */
  @Test
  public void testAddMapper_thenThrowBuildException() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.addMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addMapper(new Mapper(new Project())));
  }

  /**
   * Test {@link XSLTProcess#add(FileNameMapper)} with {@code fileNameMapper}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#add(FileNameMapper)}
   */
  @Test
  public void testAddWithFileNameMapper_thenThrowBuildException() throws BuildException {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.addMapper(new Mapper(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.add(new CutDirsMapper()));
  }

  /**
   * Test {@link XSLTProcess#execute()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) In is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#execute()}
   */
  @Test
  public void testExecute_givenXSLTProcessInIsNull_file_placeholder_thenThrowBuildException() throws BuildException {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.setIn(Copy.NULL_FILE_PLACEHOLDER);
    xsltProcess.addSysproperty(new Variable());
    xsltProcess.setXslResource(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.execute());
  }

  /**
   * Test {@link XSLTProcess#execute()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) Style is {@code style}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#execute()}
   */
  @Test
  public void testExecute_givenXSLTProcessStyleIsStyle_thenThrowBuildException() throws BuildException {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.setStyle("style");
    xsltProcess.addSysproperty(new Variable());
    xsltProcess.setXslResource(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.execute());
  }

  /**
   * Test {@link XSLTProcess#execute()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) TaskType is {@code style}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#execute()}
   */
  @Test
  public void testExecute_givenXSLTProcessTaskTypeIsStyle_thenThrowBuildException() throws BuildException {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.setTaskType("style");

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.execute());
  }

  /**
   * Test {@link XSLTProcess#execute()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#execute()}
   */
  @Test
  public void testExecute_givenXSLTProcess_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XSLTProcess()).execute());
  }

  /**
   * Test {@link XSLTProcess#createClasspath()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenXSLTProcessProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    Project project = new Project();
    xsltProcess.setProject(project);

    // Act and Assert
    assertSame(project, xsltProcess.createClasspath().getProject());
  }

  /**
   * Test {@link XSLTProcess#createClasspath()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenXSLTProcess_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new XSLTProcess()).createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link XMLCatalog} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenProject_whenXMLCatalogProjectIsProject() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(new Project());
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xsltProcess.addConfiguredXMLCatalog(xmlCatalog);

    // Assert
    Path catalogPath = xsltProcess.getXMLCatalog().getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenXMLCatalog() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xsltProcess.addConfiguredXMLCatalog(xmlCatalog);

    // Assert
    Path catalogPath = xsltProcess.getXMLCatalog().getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_givenXSLTProcessAddConfiguredXMLCatalogXMLCatalog() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xsltProcess.addConfiguredXMLCatalog(new XMLCatalog());

    // Assert that nothing has changed
    Path catalogPath = xsltProcess.getXMLCatalog().getCatalogPath();
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}.
   * <ul>
   *   <li>When {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredXMLCatalog(XMLCatalog)}
   */
  @Test
  public void testAddConfiguredXMLCatalog_whenXMLCatalog() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    // Act
    xsltProcess.addConfiguredXMLCatalog(new XMLCatalog());

    // Assert
    Path catalogPath = xsltProcess.getXMLCatalog().getCatalogPath();
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XSLTProcess#createTrace()}.
   * <p>
   * Method under test: {@link XSLTProcess#createTrace()}
   */
  @Test
  public void testCreateTrace() {
    // Arrange and Act
    TraceConfiguration actualCreateTraceResult = (new XSLTProcess()).createTrace();

    // Assert
    OutputStream outputStream = actualCreateTraceResult.getOutputStream();
    assertTrue(outputStream instanceof LogOutputStream);
    assertEquals(2, ((LogOutputStream) outputStream).getMessageLevel());
    assertFalse(actualCreateTraceResult.getElements());
    assertFalse(actualCreateTraceResult.getExtension());
    assertFalse(actualCreateTraceResult.getGeneration());
    assertFalse(actualCreateTraceResult.getSelection());
    assertFalse(actualCreateTraceResult.getTemplates());
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()}.</li>
   *   <li>When {@link Resources#Resources()} add {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenFileList_whenResourcesAddFileList() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    Resources rc = new Resources();
    rc.add(new FileList());

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(rc));
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   *   <li>When {@link Resources#Resources()} add {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenFileNameNameIsDotDot_whenResourcesAddFileList() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileName name2 = new FileName();
    name2.setName("..");

    FileList c = new FileList();
    c.addConfiguredFile(name2);
    c.addConfiguredFile(name);

    Resources rc = new Resources();
    rc.add(c);

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(rc));
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@link PolyTest.MyPath#MyPath(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenMyPathWithProjectIsProject() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    Resources rc = new Resources();
    rc.add(new MyPath(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(rc));
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@link Resources#NONE}.</li>
   *   <li>When {@link Resources#Resources()} add {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenNone_whenResourcesAddNone_thenThrowBuildException() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    Resources rc = new Resources();
    rc.add(Resources.NONE);

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(rc));
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenPathWithProjectIsProject() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    Resources rc = new Resources();
    rc.add(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(rc));
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resources#Resources()} Cache is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenTrue_whenResourcesCacheIsTrue() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    Resources rc = new Resources();
    rc.setCache(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(rc));
  }

  /**
   * Test {@link XSLTProcess#addConfiguredStyle(Resources)}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>When {@link Resources#Resources()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#addConfiguredStyle(Resources)}
   */
  @Test
  public void testAddConfiguredStyle_givenXSLTProcess_whenResources_thenThrowBuildException() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.addConfiguredStyle(new Resources()));
  }

  /**
   * Test Factory {@link Factory#addFeature(Feature)}.
   * <p>
   * Method under test: {@link Factory#addFeature(Factory.Feature)}
   */
  @Test
  public void testFactoryAddFeature() {
    // Arrange
    Factory factory = new Factory();
    Feature feature = new Feature(Manifest.ATTRIBUTE_NAME, true);

    // Act
    factory.addFeature(feature);

    // Assert
    Iterable<Feature> features = factory.getFeatures();
    assertTrue(features instanceof List);
    assertEquals(1, ((List<Feature>) features).size());
    assertSame(feature, ((List<Feature>) features).get(0));
  }

  /**
   * Test Factory getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Factory}
   *   <li>{@link Factory#setName(String)}
   *   <li>{@link Factory#getFeatures()}
   *   <li>{@link Factory#getName()}
   * </ul>
   */
  @Test
  public void testFactoryGettersAndSetters() {
    // Arrange and Act
    Factory actualFactory = new Factory();
    actualFactory.setName(Manifest.ATTRIBUTE_NAME);
    Iterable<Feature> actualFeatures = actualFactory.getFeatures();

    // Assert
    assertTrue(actualFeatures instanceof List);
    assertEquals(Manifest.ATTRIBUTE_NAME, actualFactory.getName());
  }

  /**
   * Test Factory_Attribute {@link Attribute#createDynamicElement(String)}.
   * <p>
   * Method under test: {@link Attribute#createDynamicElement(String)}
   */
  @Test
  public void testFactory_AttributeCreateDynamicElement() throws BuildException {
    // Arrange, Act and Assert
    assertNull((new Attribute()).createDynamicElement(Manifest.ATTRIBUTE_NAME));
  }

  /**
   * Test Factory_Attribute getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Attribute#getName()}
   *   <li>{@link Attribute#getValue()}
   * </ul>
   */
  @Test
  public void testFactory_AttributeGettersAndSetters() {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    String actualName = attribute.getName();

    // Assert
    assertNull(attribute.getValue());
    assertNull(actualName);
  }

  /**
   * Test Factory_Attribute new {@link Attribute} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Attribute}
   */
  @Test
  public void testFactory_AttributeNewAttribute() {
    // Arrange and Act
    Attribute actualAttribute = new Attribute();

    // Assert
    assertNull(actualAttribute.getValue());
    Location location = actualAttribute.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAttribute.getDescription());
    assertNull(actualAttribute.getName());
    assertNull(actualAttribute.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test Factory_Attribute {@link Attribute#setDynamicAttribute(String, String)}.
   * <ul>
   *   <li>Given {@link Attribute} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setDynamicAttribute(String, String)}
   */
  @Test
  public void testFactory_AttributeSetDynamicAttribute_givenAttribute_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Attribute()).setDynamicAttribute("42", "42"));
  }

  /**
   * Test Factory_Attribute {@link Attribute#setDynamicAttribute(String, String)}.
   * <ul>
   *   <li>Then {@link Attribute} (default constructor) Name is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Attribute#setDynamicAttribute(String, String)}
   */
  @Test
  public void testFactory_AttributeSetDynamicAttribute_thenAttributeNameIs42() throws BuildException {
    // Arrange
    Attribute attribute = new Attribute();

    // Act
    attribute.setDynamicAttribute(Manifest.ATTRIBUTE_NAME, "42");

    // Assert
    assertEquals("42", attribute.getName());
  }

  /**
   * Test Factory_Feature getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Factory.Feature#Feature()}
   *   <li>{@link Factory.Feature#setName(String)}
   *   <li>{@link Factory.Feature#setValue(boolean)}
   *   <li>{@link Factory.Feature#getName()}
   *   <li>{@link Factory.Feature#getValue()}
   * </ul>
   */
  @Test
  public void testFactory_FeatureGettersAndSetters() {
    // Arrange and Act
    Feature actualFeature = new Feature();
    actualFeature.setName(Manifest.ATTRIBUTE_NAME);
    actualFeature.setValue(true);
    String actualName = actualFeature.getName();

    // Assert
    assertTrue(actualFeature.getValue());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
  }

  /**
   * Test Factory_Feature getters and setters.
   * <ul>
   *   <li>When {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Factory.Feature#Feature(String, boolean)}
   *   <li>{@link Factory.Feature#setName(String)}
   *   <li>{@link Factory.Feature#setValue(boolean)}
   *   <li>{@link Factory.Feature#getName()}
   *   <li>{@link Factory.Feature#getValue()}
   * </ul>
   */
  @Test
  public void testFactory_FeatureGettersAndSetters_whenAttribute_name() {
    // Arrange and Act
    Feature actualFeature = new Feature(Manifest.ATTRIBUTE_NAME, true);
    actualFeature.setName(Manifest.ATTRIBUTE_NAME);
    actualFeature.setValue(true);
    String actualName = actualFeature.getName();

    // Assert
    assertTrue(actualFeature.getValue());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link XSLTProcess#setBasedir(File)}
   *   <li>{@link XSLTProcess#setDestdir(File)}
   *   <li>{@link XSLTProcess#setExtension(String)}
   *   <li>{@link XSLTProcess#setFailOnError(boolean)}
   *   <li>{@link XSLTProcess#setFailOnNoResources(boolean)}
   *   <li>{@link XSLTProcess#setFailOnTransformationError(boolean)}
   *   <li>{@link XSLTProcess#setFileDirParameter(String)}
   *   <li>{@link XSLTProcess#setFileNameParameter(String)}
   *   <li>{@link XSLTProcess#setForce(boolean)}
   *   <li>{@link XSLTProcess#setIn(File)}
   *   <li>{@link XSLTProcess#setOut(File)}
   *   <li>{@link XSLTProcess#setProcessor(String)}
   *   <li>{@link XSLTProcess#setScanIncludedDirectories(boolean)}
   *   <li>{@link XSLTProcess#setStyle(String)}
   *   <li>{@link XSLTProcess#setSuppressWarnings(boolean)}
   *   <li>{@link XSLTProcess#setUseImplicitFileset(boolean)}
   *   <li>{@link XSLTProcess#setXslResource(Resource)}
   *   <li>{@link XSLTProcess#getFactory()}
   *   <li>{@link XSLTProcess#getSuppressWarnings()}
   *   <li>{@link XSLTProcess#getTraceConfiguration()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    // Act
    xsltProcess.setBasedir(Copy.NULL_FILE_PLACEHOLDER);
    xsltProcess.setDestdir(Copy.NULL_FILE_PLACEHOLDER);
    xsltProcess.setExtension(Manifest.ATTRIBUTE_NAME);
    xsltProcess.setFailOnError(true);
    xsltProcess.setFailOnNoResources(true);
    xsltProcess.setFailOnTransformationError(true);
    xsltProcess.setFileDirParameter("File Dir Parameter");
    xsltProcess.setFileNameParameter("foo.txt");
    xsltProcess.setForce(true);
    xsltProcess.setIn(Copy.NULL_FILE_PLACEHOLDER);
    xsltProcess.setOut(Copy.NULL_FILE_PLACEHOLDER);
    xsltProcess.setProcessor("Processor");
    xsltProcess.setScanIncludedDirectories(true);
    xsltProcess.setStyle("Xsl File");
    xsltProcess.setSuppressWarnings(true);
    xsltProcess.setUseImplicitFileset(true);
    xsltProcess.setXslResource(new Resource());
    Factory actualFactory = xsltProcess.getFactory();
    boolean actualSuppressWarnings = xsltProcess.getSuppressWarnings();

    // Assert
    assertNull(actualFactory);
    assertNull(xsltProcess.getTraceConfiguration());
    assertTrue(actualSuppressWarnings);
  }

  /**
   * Test {@link XSLTProcess#getXMLCatalog()}.
   * <p>
   * Method under test: {@link XSLTProcess#getXMLCatalog()}
   */
  @Test
  public void testGetXMLCatalog() {
    // Arrange and Act
    XMLCatalog actualXMLCatalog = (new XSLTProcess()).getXMLCatalog();

    // Assert
    Location location = actualXMLCatalog.getLocation();
    assertNull(location.getFileName());
    assertNull(actualXMLCatalog.getDescription());
    assertNull(actualXMLCatalog.getProject());
    assertNull(actualXMLCatalog.getCatalogPath());
    assertNull(actualXMLCatalog.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualXMLCatalog.isReference());
  }

  /**
   * Test {@link XSLTProcess#getLiaison()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) Processor is {@code Processor}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#getLiaison()}
   */
  @Test
  public void testGetLiaison_givenXSLTProcessProcessorIsProcessor_thenThrowBuildException() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.setProcessor("Processor");

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.getLiaison());
  }

  /**
   * Test {@link XSLTProcess#getLiaison()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor) Processor is {@link XSLTProcess#PROCESSOR_TRAX}.</li>
   *   <li>Then return {@link TraXLiaison}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#getLiaison()}
   */
  @Test
  public void testGetLiaison_givenXSLTProcessProcessorIsProcessor_trax_thenReturnTraXLiaison() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();
    xsltProcess.setProcessor(XSLTProcess.PROCESSOR_TRAX);

    // Act and Assert
    assertTrue(xsltProcess.getLiaison() instanceof TraXLiaison);
  }

  /**
   * Test {@link XSLTProcess#getLiaison()}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>Then return {@link TraXLiaison}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#getLiaison()}
   */
  @Test
  public void testGetLiaison_givenXSLTProcess_thenReturnTraXLiaison() {
    // Arrange, Act and Assert
    assertTrue((new XSLTProcess()).getLiaison() instanceof TraXLiaison);
  }

  /**
   * Test {@link XSLTProcess#createParam()}.
   * <p>
   * Method under test: {@link XSLTProcess#createParam()}
   */
  @Test
  public void testCreateParam() {
    // Arrange, Act and Assert
    assertNull((new XSLTProcess()).createParam().getType());
  }

  /**
   * Test {@link XSLTProcess#createOutputProperty()}.
   * <p>
   * Method under test: {@link XSLTProcess#createOutputProperty()}
   */
  @Test
  public void testCreateOutputProperty() {
    // Arrange and Act
    OutputProperty actualCreateOutputPropertyResult = (new XSLTProcess()).createOutputProperty();

    // Assert
    assertNull(actualCreateOutputPropertyResult.getName());
    assertNull(actualCreateOutputPropertyResult.getValue());
  }

  /**
   * Test {@link XSLTProcess#configureLiaison(File)} with {@code File}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#configureLiaison(File)}
   */
  @Test
  public void testConfigureLiaisonWithFile_givenXSLTProcess_whenNull_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XSLTProcess()).configureLiaison((File) null));
  }

  /**
   * Test {@link XSLTProcess#configureLiaison(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()} BaseDir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#configureLiaison(Resource)}
   */
  @Test
  public void testConfigureLiaisonWithResource_whenFileResourceBaseDirIsNull_file_placeholder() throws BuildException {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    FileResource stylesheet = new FileResource();
    stylesheet.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.configureLiaison(stylesheet));
  }

  /**
   * Test {@link XSLTProcess#configureLiaison(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#configureLiaison(Resource)}
   */
  @Test
  public void testConfigureLiaisonWithResource_whenFileResource_thenThrowBuildException() throws BuildException {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.configureLiaison(new FileResource()));
  }

  /**
   * Test {@link XSLTProcess#createFactory()}.
   * <p>
   * Method under test: {@link XSLTProcess#createFactory()}
   */
  @Test
  public void testCreateFactory() throws BuildException {
    // Arrange and Act
    Factory actualCreateFactoryResult = (new XSLTProcess()).createFactory();

    // Assert
    Iterable<Feature> features = actualCreateFactoryResult.getFeatures();
    assertTrue(features instanceof List);
    assertNull(actualCreateFactoryResult.getName());
    assertTrue(((List<Feature>) features).isEmpty());
  }

  /**
   * Test {@link XSLTProcess#handleError(Throwable)} with {@code ex}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#handleError(Throwable)}
   */
  @Test
  public void testHandleErrorWithEx_givenXSLTProcess_thenThrowBuildException() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.handleError(new Throwable()));
  }

  /**
   * Test {@link XSLTProcess#handleError(String)} with {@code msg}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#handleError(String)}
   */
  @Test
  public void testHandleErrorWithMsg_givenXSLTProcess_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new XSLTProcess()).handleError("Msg"));
  }

  /**
   * Test {@link XSLTProcess#handleTransformationError(Exception)}.
   * <ul>
   *   <li>Given {@link XSLTProcess} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XSLTProcess#handleTransformationError(Exception)}
   */
  @Test
  public void testHandleTransformationError_givenXSLTProcess_thenThrowBuildException() {
    // Arrange
    XSLTProcess xsltProcess = new XSLTProcess();

    // Act and Assert
    assertThrows(BuildException.class, () -> xsltProcess.handleTransformationError(new Exception("foo")));
  }

  /**
   * Test new {@link XSLTProcess} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XSLTProcess}
   */
  @Test
  public void testNewXSLTProcess() {
    // Arrange and Act
    XSLTProcess actualXsltProcess = new XSLTProcess();

    // Assert
    assertTrue(actualXsltProcess.getLiaison() instanceof TraXLiaison);
    assertNull(actualXsltProcess.getDescription());
    assertNull(actualXsltProcess.getTaskName());
    assertNull(actualXsltProcess.getTaskType());
    assertNull(actualXsltProcess.getProject());
    assertNull(actualXsltProcess.getOwningTarget());
    assertNull(actualXsltProcess.getFactory());
    assertNull(actualXsltProcess.getTraceConfiguration());
    assertFalse(actualXsltProcess.hasSelectors());
    assertFalse(actualXsltProcess.getSuppressWarnings());
  }

  /**
   * Test OutputProperty getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link OutputProperty}
   *   <li>{@link OutputProperty#setName(String)}
   *   <li>{@link OutputProperty#setValue(String)}
   *   <li>{@link OutputProperty#getName()}
   *   <li>{@link OutputProperty#getValue()}
   * </ul>
   */
  @Test
  public void testOutputPropertyGettersAndSetters() {
    // Arrange and Act
    OutputProperty actualOutputProperty = new OutputProperty();
    actualOutputProperty.setName(Manifest.ATTRIBUTE_NAME);
    actualOutputProperty.setValue("42");
    String actualName = actualOutputProperty.getName();

    // Assert
    assertEquals("42", actualOutputProperty.getValue());
    assertEquals(Manifest.ATTRIBUTE_NAME, actualName);
  }

  /**
   * Test Param {@link Param#getExpression()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) Expression is {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#getExpression()}
   */
  @Test
  public void testParamGetExpression_givenParamExpressionIsFoo_thenReturnFoo() throws BuildException {
    // Arrange
    Param param = new Param();
    param.setExpression("foo");

    // Act and Assert
    assertEquals("foo", param.getExpression());
  }

  /**
   * Test Param {@link Param#getExpression()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#getExpression()}
   */
  @Test
  public void testParamGetExpression_givenParam_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Param()).getExpression());
  }

  /**
   * Test Param {@link Param#getName()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) Name is {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#getName()}
   */
  @Test
  public void testParamGetName_givenParamNameIsFoo_thenReturnFoo() throws BuildException {
    // Arrange
    Param param = new Param();
    param.setName("foo");

    // Act and Assert
    assertEquals("foo", param.getName());
  }

  /**
   * Test Param {@link Param#getName()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#getName()}
   */
  @Test
  public void testParamGetName_givenParam_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Param()).getName());
  }

  /**
   * Test Param getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Param}
   *   <li>{@link Param#setExpression(String)}
   *   <li>{@link Param#setIf(Object)}
   *   <li>{@link Param#setName(String)}
   *   <li>{@link Param#setProject(Project)}
   *   <li>{@link Param#setType(String)}
   *   <li>{@link Param#setUnless(Object)}
   *   <li>{@link Param#getType()}
   * </ul>
   */
  @Test
  public void testParamGettersAndSetters() throws BuildException {
    // Arrange and Act
    Param actualParam = new Param();
    actualParam.setExpression("Expression");
    actualParam.setIf((Object) "If Cond");
    actualParam.setName(Manifest.ATTRIBUTE_NAME);
    actualParam.setProject(new Project());
    actualParam.setType("Type");
    actualParam.setUnless((Object) "Unless Cond");
    String actualType = actualParam.getType();

    // Assert
    assertEquals("Expression", actualParam.getExpression());
    assertEquals("Type", actualType);
    assertEquals(Manifest.ATTRIBUTE_NAME, actualParam.getName());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code 42}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIs42_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "42");
    param.setProject(null);
    param.setUnless((Object) null);

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsAntRefid_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "ant.refid:");
    param.setProject(null);
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code ant.refid:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsAntRefid_thenReturnFalse2() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "ant.refid:");
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@link Boolean#FALSE} toString.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsFalseToString_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) Boolean.FALSE.toString());
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code no}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsNo_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "no");
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsNull_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) null);
    param.setProject(null);
    param.setUnless((Object) "");

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code off}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsOff_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "off");
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code on}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsOn_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "on");
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsOne_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf(1);
    param.setProject(null);
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsToString_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "toString:");
    param.setProject(null);
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code toString:}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsToString_thenReturnFalse2() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "toString:");
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@link Boolean#TRUE} toString.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsTrueToString_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) Boolean.TRUE.toString());
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsTrue_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf(true);
    param.setProject(null);
    param.setUnless((Object) "");

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) If is {@code yes}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamIfIsYes_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) "yes");
    param.setProject(new Project());
    param.setUnless((Object) "");

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) Unless is {@code 42}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamUnlessIs42_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) null);
    param.setProject(null);
    param.setUnless((Object) "42");

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) Unless is {@code null}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamUnlessIsNull_thenReturnTrue() {
    // Arrange
    Param param = new Param();
    param.setIf((Object) null);
    param.setProject(new Project());
    param.setUnless((Object) null);

    // Act and Assert
    assertTrue(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor) Unless is {@code true}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParamUnlessIsTrue_thenReturnFalse() {
    // Arrange
    Param param = new Param();
    param.setIf(true);
    param.setProject(null);
    param.setUnless(true);

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Param} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenParam_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Param()).shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenProjectAddBuildListenerAntClassLoader_thenReturnFalse() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Param param = new Param();
    param.setIf((Object) "toString:");
    param.setProject(project);
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test Param {@link Param#shouldUse()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code Adding reference:} and {@link Target#Target()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Param#shouldUse()}
   */
  @Test
  public void testParamShouldUse_givenProjectAddTargetAddingReferenceAndTarget_thenReturnFalse() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("Adding reference: ", new Target());
    project.addBuildListener(new AntClassLoader());

    Param param = new Param();
    param.setIf((Object) "toString:");
    param.setProject(project);
    param.setUnless((Object) "");

    // Act and Assert
    assertFalse(param.shouldUse());
  }

  /**
   * Test TraceConfiguration {@link TraceConfiguration#getOutputStream()}.
   * <p>
   * Method under test: {@link TraceConfiguration#getOutputStream()}
   */
  @Test
  public void testTraceConfigurationGetOutputStream() {
    // Arrange and Act
    OutputStream actualOutputStream = ((new XSLTProcess()).new TraceConfiguration()).getOutputStream();

    // Assert
    assertTrue(actualOutputStream instanceof LogOutputStream);
    assertEquals(2, ((LogOutputStream) actualOutputStream).getMessageLevel());
  }

  /**
   * Test TraceConfiguration getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link TraceConfiguration#TraceConfiguration(XSLTProcess)}
   *   <li>{@link TraceConfiguration#setElements(boolean)}
   *   <li>{@link TraceConfiguration#setExtension(boolean)}
   *   <li>{@link TraceConfiguration#setGeneration(boolean)}
   *   <li>{@link TraceConfiguration#setSelection(boolean)}
   *   <li>{@link TraceConfiguration#setTemplates(boolean)}
   *   <li>{@link TraceConfiguration#getElements()}
   *   <li>{@link TraceConfiguration#getExtension()}
   *   <li>{@link TraceConfiguration#getGeneration()}
   *   <li>{@link TraceConfiguration#getSelection()}
   *   <li>{@link TraceConfiguration#getTemplates()}
   * </ul>
   */
  @Test
  public void testTraceConfigurationGettersAndSetters() {
    // Arrange and Act
    TraceConfiguration actualTraceConfiguration = (new XSLTProcess()).new TraceConfiguration();
    actualTraceConfiguration.setElements(true);
    actualTraceConfiguration.setExtension(true);
    actualTraceConfiguration.setGeneration(true);
    actualTraceConfiguration.setSelection(true);
    actualTraceConfiguration.setTemplates(true);
    boolean actualElements = actualTraceConfiguration.getElements();
    boolean actualExtension = actualTraceConfiguration.getExtension();
    boolean actualGeneration = actualTraceConfiguration.getGeneration();
    boolean actualSelection = actualTraceConfiguration.getSelection();

    // Assert
    assertTrue(actualElements);
    assertTrue(actualExtension);
    assertTrue(actualGeneration);
    assertTrue(actualSelection);
    assertTrue(actualTraceConfiguration.getTemplates());
  }
}
