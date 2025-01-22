package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Paths;
import java.util.Stack;
import java.util.Vector;
import javax.xml.transform.Source;
import javax.xml.transform.TransformerException;
import javax.xml.transform.sax.SAXSource;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.BuildListener;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.junit.Test;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class XMLCatalogDiffblueTest {
  /**
   * Test new {@link XMLCatalog} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link XMLCatalog}
   */
  @Test
  public void testNewXMLCatalog() {
    // Arrange and Act
    XMLCatalog actualXmlCatalog = new XMLCatalog();

    // Assert
    assertEquals("XMLCatalog", actualXmlCatalog.getDataTypeName());
    Location location = actualXmlCatalog.getLocation();
    assertNull(location.getFileName());
    assertNull(actualXmlCatalog.getDescription());
    assertNull(actualXmlCatalog.getProject());
    assertNull(actualXmlCatalog.getCatalogPath());
    assertNull(actualXmlCatalog.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualXmlCatalog.isChecked());
    assertFalse(actualXmlCatalog.isReference());
  }

  /**
   * Test {@link XMLCatalog#createClasspath()}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   *   <li>Then return DataTypeName is {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenXMLCatalog_thenReturnDataTypeNameIsPath() {
    // Arrange and Act
    Path actualCreateClasspathResult = (new XMLCatalog()).createClasspath();

    // Assert
    assertEquals("Path", actualCreateClasspathResult.getDataTypeName());
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertTrue(actualCreateClasspathResult.isChecked());
    assertTrue(actualCreateClasspathResult.isEmpty());
  }

  /**
   * Test {@link XMLCatalog#createCatalogPath()}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#createCatalogPath()}
   */
  @Test
  public void testCreateCatalogPath_givenXMLCatalogAddConfiguredXMLCatalogXMLCatalog() {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlCatalog.createCatalogPath();

    // Assert that nothing has changed
    Path catalogPath = xmlCatalog.getCatalogPath();
    assertEquals("Path", catalogPath.getDataTypeName());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isChecked());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XMLCatalog#createCatalogPath()}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#createCatalogPath()}
   */
  @Test
  public void testCreateCatalogPath_givenXMLCatalogProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    Project project = new Project();
    xmlCatalog.setProject(project);

    // Act and Assert
    assertSame(project, xmlCatalog.createCatalogPath().getProject());
  }

  /**
   * Test {@link XMLCatalog#createCatalogPath()}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   *   <li>Then {@link XMLCatalog} (default constructor) CatalogPath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#createCatalogPath()}
   */
  @Test
  public void testCreateCatalogPath_givenXMLCatalog_thenXMLCatalogCatalogPathDescriptionIsNull() {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();

    // Act
    Path actualCreateCatalogPathResult = xmlCatalog.createCatalogPath();

    // Assert
    Path catalogPath = xmlCatalog.getCatalogPath();
    assertEquals("Path", catalogPath.getDataTypeName());
    assertNull(catalogPath.getDescription());
    assertNull(actualCreateCatalogPathResult.getProject());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertEquals(0, catalogPath.size());
    assertFalse(catalogPath.isChecked());
    assertFalse(catalogPath.isReference());
    assertTrue(catalogPath.isEmpty());
  }

  /**
   * Test {@link XMLCatalog#setCatalogPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#setCatalogPathRef(Reference)}
   */
  @Test
  public void testSetCatalogPathRef_givenXMLCatalogAddConfiguredXMLCatalogXMLCatalog() {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());

    // Act
    xmlCatalog.setCatalogPathRef(new Reference("42"));

    // Assert that nothing has changed
    Path catalogPath = xmlCatalog.getCatalogPath();
    assertEquals("Path", catalogPath.getDataTypeName());
    assertFalse(catalogPath.isChecked());
    assertFalse(catalogPath.isReference());
  }

  /**
   * Test {@link XMLCatalog#setCatalogPathRef(Reference)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   *   <li>Then {@link XMLCatalog} (default constructor) CatalogPath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#setCatalogPathRef(Reference)}
   */
  @Test
  public void testSetCatalogPathRef_givenXMLCatalog_thenXMLCatalogCatalogPathProjectIsNull() {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();

    // Act
    xmlCatalog.setCatalogPathRef(new Reference("42"));

    // Assert
    Path catalogPath = xmlCatalog.getCatalogPath();
    assertEquals("Path", catalogPath.getDataTypeName());
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getProject());
    assertNull(catalogPath.getRefid());
    assertFalse(catalogPath.isChecked());
    assertFalse(catalogPath.isReference());
  }

  /**
   * Test {@link XMLCatalog#setCatalogPathRef(Reference)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) CatalogPath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#setCatalogPathRef(Reference)}
   */
  @Test
  public void testSetCatalogPathRef_thenXMLCatalogCatalogPathProjectIsProject() {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    Project project = new Project();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.setCatalogPathRef(new Reference("42"));

    // Assert
    Path catalogPath = xmlCatalog.getCatalogPath();
    assertEquals("Path", catalogPath.getDataTypeName());
    assertNull(catalogPath.getDescription());
    assertNull(catalogPath.getRefid());
    assertFalse(catalogPath.isChecked());
    assertFalse(catalogPath.isReference());
    assertSame(project, catalogPath.getProject());
  }

  /**
   * Test {@link XMLCatalog#getCatalogPath()}.
   * <p>
   * Method under test: {@link XMLCatalog#getCatalogPath()}
   */
  @Test
  public void testGetCatalogPath() {
    // Arrange, Act and Assert
    assertNull((new XMLCatalog()).getCatalogPath());
  }

  /**
   * Test {@link XMLCatalog#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   *   <li>Then {@link XMLCatalog} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenXMLCatalog_thenXMLCatalogReference() throws BuildException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    Reference r = new Reference("42");

    // Act
    xmlCatalog.setRefid(r);

    // Assert
    assertTrue(xmlCatalog.isReference());
    assertSame(r, xmlCatalog.getRefid());
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity() throws IOException, SAXException {
    // Arrange
    AntClassLoader coreLoader = new AntClassLoader();
    coreLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Project project = new Project();
    project.setCoreLoader(coreLoader);
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity2() throws IOException, SAXException {
    // Arrange
    AntClassLoader coreLoader = new AntClassLoader();
    coreLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "resolveEntity: '").toFile());

    Project project = new Project();
    project.setCoreLoader(coreLoader);
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenProjectCoreLoaderIsAntClassLoader() throws IOException, SAXException {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link ResourceLocation} (default constructor) Location is {@code '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenResourceLocationLocationIsApostrophe()
      throws IOException, BuildException, SAXException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("': '");
    dtd.setPublicId("resolveEntity: '");

    ResourceLocation dtd2 = new ResourceLocation();
    dtd2.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd2.setLocation("'");
    dtd2.setPublicId("': '");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd2);
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link ResourceLocation} (default constructor) Location is {@code resolveEntity: '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenResourceLocationLocationIsResolveEntity()
      throws IOException, BuildException, SAXException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("resolveEntity: '");
    dtd.setPublicId("42");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd);
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    assertEquals(3, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link ResourceLocation} (default constructor) PublicId is {@code resolveEntity: '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenResourceLocationPublicIdIsResolveEntity()
      throws IOException, BuildException, SAXException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("': '");
    dtd.setPublicId("resolveEntity: '");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenXMLCatalogAddConfiguredXMLCatalogXMLCatalog() throws IOException, SAXException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenXMLCatalogAddConfiguredXMLCatalogXMLCatalog2() throws IOException, SAXException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addEntity {@link ResourceLocation} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_givenXMLCatalogAddEntityResourceLocation()
      throws IOException, BuildException, SAXException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ResourceLocation entity = new ResourceLocation();
    entity.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    entity.setLocation("': '");
    entity.setPublicId("42");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addEntity(entity);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    assertEquals(3, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) Project BuildListeners first {@link DefaultLogger}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_thenXMLCatalogProjectBuildListenersFirstDefaultLogger()
      throws IOException, SAXException {
    // Arrange
    Project project = new Project();
    DefaultLogger listener = new DefaultLogger();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    InputSource actualResolveEntityResult = xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    BuildListener getResult = buildListeners.get(0);
    assertTrue(getResult instanceof DefaultLogger);
    assertNull(actualResolveEntityResult);
    assertSame(listener, getResult);
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_thenXMLCatalogProjectBuildListenersFirstIsAntClassLoader()
      throws IOException, SAXException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_thenXMLCatalogProjectBuildListenersSizeIsOne() throws IOException, SAXException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(new Project());

    // Act and Assert
    assertNull(xmlCatalog.resolveEntity("42", "42"));
    assertEquals(1, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) Project BuildListeners size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_thenXMLCatalogProjectBuildListenersSizeIsThree()
      throws IOException, BuildException, SAXException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("': '");
    dtd.setPublicId("42");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    assertEquals(3, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolveEntity(String, String)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) Project BuildListeners size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolveEntity(String, String)}
   */
  @Test
  public void testResolveEntity_thenXMLCatalogProjectBuildListenersSizeIsThree2()
      throws IOException, BuildException, SAXException {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    project.addBuildListener(new AntClassLoader());

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("': '");
    dtd.setPublicId("42");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolveEntity("42", "42");

    // Assert
    assertEquals(3, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve() throws TransformerException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(new Project());

    // Act
    Source actualResolveResult = xmlCatalog.resolve("Href", null);

    // Assert
    assertTrue(actualResolveResult instanceof SAXSource);
    assertEquals(1, xmlCatalog.getProject().getBuildListeners().size());
    String expectedSystemId = String.join("", "file:", Paths.get(System.getProperty("user.dir"), "Href").toString());
    assertEquals(expectedSystemId, actualResolveResult.getSystemId());
    String expectedSystemId2 = String.join("", "file:", Paths.get(System.getProperty("user.dir"), "Href").toString());
    assertEquals(expectedSystemId2, ((SAXSource) actualResolveResult).getInputSource().getSystemId());
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve2() throws TransformerException {
    // Arrange
    AntClassLoader coreLoader = new AntClassLoader();
    coreLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    Project project = new Project();
    project.setCoreLoader(coreLoader);
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve3() throws TransformerException {
    // Arrange
    AntClassLoader coreLoader = new AntClassLoader();
    coreLoader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "#").toFile());

    Project project = new Project();
    project.setCoreLoader(coreLoader);
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>Then return SystemId is {@code Href}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenProjectAddBuildListenerDefaultLogger_thenReturnSystemIdIsHref()
      throws TransformerException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    Source actualResolveResult = xmlCatalog.resolve("Href", "Base");

    // Assert
    assertTrue(actualResolveResult instanceof SAXSource);
    assertEquals("Href", actualResolveResult.getSystemId());
    assertEquals("Href", ((SAXSource) actualResolveResult).getInputSource().getSystemId());
    assertEquals(2, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) CoreLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenProjectCoreLoaderIsAntClassLoader() throws TransformerException {
    // Arrange
    Project project = new Project();
    project.setCoreLoader(new AntClassLoader());
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link ResourceLocation} (default constructor) Location is {@code ' with base: '}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenResourceLocationLocationIsWithBase()
      throws MalformedURLException, TransformerException, BuildException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("resolve: '");
    dtd.setPublicId("#");

    ResourceLocation dtd2 = new ResourceLocation();
    dtd2.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd2.setLocation("' with base: '");
    dtd2.setPublicId("resolve: '");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd2);
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link ResourceLocation} (default constructor) PublicId is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenResourceLocationPublicIdIsEmptyString()
      throws MalformedURLException, TransformerException, BuildException {
    // Arrange
    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("resolve: '");
    dtd.setPublicId("");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(new Project());

    // Act
    Source actualResolveResult = xmlCatalog.resolve("#", "Base");

    // Assert
    assertTrue(actualResolveResult instanceof SAXSource);
    assertEquals("", actualResolveResult.getSystemId());
    assertEquals("", ((SAXSource) actualResolveResult).getInputSource().getSystemId());
    assertEquals(2, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link ResourceLocation} (default constructor) PublicId is {@code #}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenResourceLocationPublicIdIsNumberSign()
      throws MalformedURLException, TransformerException, BuildException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    ResourceLocation dtd = new ResourceLocation();
    dtd.setBase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL());
    dtd.setLocation("resolve: '");
    dtd.setPublicId("#");

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addDTD(dtd);
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenXMLCatalogAddConfiguredXMLCatalogXMLCatalog() throws TransformerException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) addConfiguredXMLCatalog {@link XMLCatalog} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenXMLCatalogAddConfiguredXMLCatalogXMLCatalog2() throws TransformerException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>When {@code Href}.</li>
   *   <li>Then return SystemId is {@code Href}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_givenXMLCatalogProjectIsProject_whenHref_thenReturnSystemIdIsHref()
      throws TransformerException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(new Project());

    // Act
    Source actualResolveResult = xmlCatalog.resolve("Href", "Base");

    // Assert
    assertTrue(actualResolveResult instanceof SAXSource);
    assertEquals("Href", actualResolveResult.getSystemId());
    assertEquals("Href", ((SAXSource) actualResolveResult).getInputSource().getSystemId());
    assertEquals(1, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) Project BuildListeners first is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_thenXMLCatalogProjectBuildListenersFirstIsAntClassLoader() throws TransformerException {
    // Arrange
    Project project = new Project();
    AntClassLoader listener = new AntClassLoader();
    project.addBuildListener(listener);

    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(project);

    // Act
    xmlCatalog.resolve("Href", "Base");

    // Assert
    Vector<BuildListener> buildListeners = xmlCatalog.getProject().getBuildListeners();
    assertEquals(2, buildListeners.size());
    assertSame(listener, buildListeners.get(0));
  }

  /**
   * Test {@link XMLCatalog#resolve(String, String)}.
   * <ul>
   *   <li>When {@code #}.</li>
   *   <li>Then return SystemId is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#resolve(String, String)}
   */
  @Test
  public void testResolve_whenNumberSign_thenReturnSystemIdIsEmptyString() throws TransformerException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setProject(new Project());

    // Act
    Source actualResolveResult = xmlCatalog.resolve("#", "Base");

    // Assert
    assertTrue(actualResolveResult instanceof SAXSource);
    assertEquals("", actualResolveResult.getSystemId());
    assertEquals("", ((SAXSource) actualResolveResult).getInputSource().getSystemId());
    assertEquals(1, xmlCatalog.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link XMLCatalog#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd42() throws BuildException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();

    Stack<Object> stk = new Stack<>();
    stk.add("42");

    // Act
    xmlCatalog.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(xmlCatalog.isChecked());
  }

  /**
   * Test {@link XMLCatalog#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link Stack} (default constructor) add {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_given42_whenStackAdd422() throws BuildException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();

    Stack<Object> stk = new Stack<>();
    stk.add("42");
    stk.add("42");

    // Act
    xmlCatalog.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(xmlCatalog.isChecked());
  }

  /**
   * Test {@link XMLCatalog#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor) Checked is {@code true}.</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenXMLCatalogCheckedIsTrue_whenStack() throws BuildException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.setChecked(true);
    Stack<Object> stk = new Stack<>();

    // Act
    xmlCatalog.dieOnCircularReference(stk, new Project());

    // Assert that nothing has changed
    assertTrue(xmlCatalog.isChecked());
  }

  /**
   * Test {@link XMLCatalog#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Given {@link XMLCatalog} (default constructor).</li>
   *   <li>When {@link Stack} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_givenXMLCatalog_whenStack() throws BuildException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    Stack<Object> stk = new Stack<>();

    // Act
    xmlCatalog.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(xmlCatalog.isChecked());
  }

  /**
   * Test {@link XMLCatalog#dieOnCircularReference(Stack, Project)} with {@code stk}, {@code p}.
   * <ul>
   *   <li>Then {@link XMLCatalog} (default constructor) CatalogPath Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link XMLCatalog#dieOnCircularReference(Stack, Project)}
   */
  @Test
  public void testDieOnCircularReferenceWithStkP_thenXMLCatalogCatalogPathChecked() throws BuildException {
    // Arrange
    XMLCatalog xmlCatalog = new XMLCatalog();
    xmlCatalog.addConfiguredXMLCatalog(new XMLCatalog());
    Stack<Object> stk = new Stack<>();

    // Act
    xmlCatalog.dieOnCircularReference(stk, new Project());

    // Assert
    assertTrue(xmlCatalog.isChecked());
    assertTrue(xmlCatalog.getCatalogPath().isChecked());
  }
}
