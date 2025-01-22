package org.apache.tools.ant.helper;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.helper.ProjectHelperImpl.RootHandler;
import org.junit.Test;
import org.xml.sax.AttributeList;
import org.xml.sax.SAXParseException;
import org.xml.sax.helpers.AttributeListImpl;

public class ProjectHelperImplDiffblueTest {
  /**
   * Test new {@link ProjectHelperImpl} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ProjectHelperImpl}
   */
  @Test
  public void testNewProjectHelperImpl() {
    // Arrange and Act
    ProjectHelperImpl actualProjectHelperImpl = new ProjectHelperImpl();

    // Assert
    assertEquals("build.xml", actualProjectHelperImpl.getDefaultBuildFile());
    assertTrue(actualProjectHelperImpl.getExtensionStack().isEmpty());
    assertTrue(actualProjectHelperImpl.getImportStack().isEmpty());
  }

  /**
   * Test {@link ProjectHelperImpl#parse(Project, Object)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperImpl#parse(Project, Object)}
   */
  @Test
  public void testParse_givenAntClassLoader_whenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    ProjectHelperImpl projectHelperImpl = new ProjectHelperImpl();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelperImpl.parse(project, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ProjectHelperImpl#parse(Project, Object)}.
   * <ul>
   *   <li>Given {@link DefaultLogger} (default constructor).</li>
   *   <li>When {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperImpl#parse(Project, Object)}
   */
  @Test
  public void testParse_givenDefaultLogger_whenProjectAddBuildListenerDefaultLogger() throws BuildException {
    // Arrange
    ProjectHelperImpl projectHelperImpl = new ProjectHelperImpl();

    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelperImpl.parse(project, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ProjectHelperImpl#parse(Project, Object)}.
   * <ul>
   *   <li>Given {@code with URI =}.</li>
   *   <li>When {@link Project} (default constructor) addDataTypeDefinition {@code with URI =} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperImpl#parse(Project, Object)}
   */
  @Test
  public void testParse_givenWithUri_whenProjectAddDataTypeDefinitionWithUriAndObject() throws BuildException {
    // Arrange
    ProjectHelperImpl projectHelperImpl = new ProjectHelperImpl();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(" with URI = ", typeClass);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelperImpl.parse(project, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ProjectHelperImpl#parse(Project, Object)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperImpl#parse(Project, Object)}
   */
  @Test
  public void testParse_whenProject_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelperImpl projectHelperImpl = new ProjectHelperImpl();
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> projectHelperImpl.parse(project, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ProjectHelperImpl#parse(Project, Object)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code parsing buildfile} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperImpl#parse(Project, Object)}
   */
  @Test
  public void testParse_whenPropertyIsJavaIoTmpdirIsParsingBuildfileToFile() throws BuildException {
    // Arrange
    ProjectHelperImpl projectHelperImpl = new ProjectHelperImpl();
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelperImpl.parse(project,
        Paths.get(System.getProperty("java.io.tmpdir"), "parsing buildfile ").toFile()));
  }

  /**
   * Test {@link ProjectHelperImpl#parse(Project, Object)}.
   * <ul>
   *   <li>When {@code Source}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ProjectHelperImpl#parse(Project, Object)}
   */
  @Test
  public void testParse_whenSource_thenThrowBuildException() throws BuildException {
    // Arrange
    ProjectHelperImpl projectHelperImpl = new ProjectHelperImpl();

    // Act and Assert
    assertThrows(BuildException.class, () -> projectHelperImpl.parse(new Project(), "Source"));
  }

  /**
   * Test RootHandler {@link RootHandler#RootHandler(ProjectHelperImpl)}.
   * <p>
   * Method under test: {@link RootHandler#RootHandler(ProjectHelperImpl)}
   */
  @Test
  public void testRootHandlerNewRootHandler() {
    // Arrange, Act and Assert
    ProjectHelperImpl projectHelperImpl = (new RootHandler(new ProjectHelperImpl())).helperImpl;
    assertEquals("build.xml", projectHelperImpl.getDefaultBuildFile());
    assertTrue(projectHelperImpl.getExtensionStack().isEmpty());
  }

  /**
   * Test RootHandler {@link RootHandler#startElement(String, AttributeList)}.
   * <ul>
   *   <li>Then throw {@link SAXParseException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RootHandler#startElement(String, AttributeList)}
   */
  @Test
  public void testRootHandlerStartElement_thenThrowSAXParseException() throws SAXParseException {
    // Arrange
    RootHandler rootHandler = new RootHandler(new ProjectHelperImpl());

    // Act and Assert
    assertThrows(SAXParseException.class, () -> rootHandler.startElement("Tag", new AttributeListImpl()));
  }
}
