package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.listener.BigProjectLogger;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class DefBaseDiffblueTest {
  /**
   * Test {@link DefBase#hasCpDelegate()}.
   * <p>
   * Method under test: {@link DefBase#hasCpDelegate()}
   */
  @Test
  public void testHasCpDelegate() {
    // Arrange, Act and Assert
    assertFalse((new Componentdef()).hasCpDelegate());
  }

  /**
   * Test {@link DefBase#setReverseLoader(boolean)}.
   * <ul>
   *   <li>Given {@link ByteArrayOutputStream#ByteArrayOutputStream(int)} with one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setReverseLoader(boolean)}
   */
  @Test
  public void testSetReverseLoader_givenByteArrayOutputStreamWithOne() {
    // Arrange
    BigProjectLogger listener = new BigProjectLogger();
    listener.setOutputPrintStream(new PrintStream(new ByteArrayOutputStream(1)));

    Project project = new Project();
    project.addBuildListener(listener);

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act
    componentdef.setReverseLoader(true);

    // Assert
    assertTrue(componentdef.isReverseLoader());
  }

  /**
   * Test {@link DefBase#setReverseLoader(boolean)}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor).</li>
   *   <li>Then {@link Componentdef} (default constructor) ReverseLoader.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setReverseLoader(boolean)}
   */
  @Test
  public void testSetReverseLoader_givenComponentdef_thenComponentdefReverseLoader() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setReverseLoader(true);

    // Assert
    assertTrue(componentdef.isReverseLoader());
  }

  /**
   * Test {@link DefBase#setReverseLoader(boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setReverseLoader(boolean)}
   */
  @Test
  public void testSetReverseLoader_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act
    componentdef.setReverseLoader(true);

    // Assert
    assertTrue(componentdef.isReverseLoader());
  }

  /**
   * Test {@link DefBase#setReverseLoader(boolean)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addDataTypeDefinition {@code Type Name} and {@link Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setReverseLoader(boolean)}
   */
  @Test
  public void testSetReverseLoader_givenProjectAddDataTypeDefinitionTypeNameAndObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act
    componentdef.setReverseLoader(true);

    // Assert
    assertTrue(componentdef.isReverseLoader());
  }

  /**
   * Test {@link DefBase#setReverseLoader(boolean)}.
   * <ul>
   *   <li>Then {@link Componentdef} (default constructor) ReverseLoader.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setReverseLoader(boolean)}
   */
  @Test
  public void testSetReverseLoader_thenComponentdefReverseLoader() {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setProject(new Project());

    // Act
    componentdef.setReverseLoader(true);

    // Assert
    assertTrue(componentdef.isReverseLoader());
  }

  /**
   * Test {@link DefBase#getClasspath()}.
   * <p>
   * Method under test: {@link DefBase#getClasspath()}
   */
  @Test
  public void testGetClasspath() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getClasspath());
  }

  /**
   * Test {@link DefBase#isReverseLoader()}.
   * <p>
   * Method under test: {@link DefBase#isReverseLoader()}
   */
  @Test
  public void testIsReverseLoader() {
    // Arrange, Act and Assert
    assertFalse((new Componentdef()).isReverseLoader());
  }

  /**
   * Test {@link DefBase#getLoaderId()}.
   * <p>
   * Method under test: {@link DefBase#getLoaderId()}
   */
  @Test
  public void testGetLoaderId() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getLoaderId());
  }

  /**
   * Test {@link DefBase#getClasspathId()}.
   * <p>
   * Method under test: {@link DefBase#getClasspathId()}
   */
  @Test
  public void testGetClasspathId() {
    // Arrange, Act and Assert
    assertNull((new Componentdef()).getClasspathId());
  }

  /**
   * Test {@link DefBase#createClasspath()}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenComponentdefProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    Componentdef componentdef = new Componentdef();
    Project project = new Project();
    componentdef.setProject(project);

    // Act and Assert
    assertSame(project, componentdef.createClasspath().getProject());
  }

  /**
   * Test {@link DefBase#createClasspath()}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenComponentdef_thenReturnLocationFileNameIsNull() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    Path actualCreateClasspathResult = componentdef.createClasspath();

    // Assert
    Location location = actualCreateClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateClasspathResult.getDescription());
    Path classpath = componentdef.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(actualCreateClasspathResult.getRefid());
    assertNull(classpath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateClasspathResult.size());
    assertEquals(0, classpath.size());
    assertFalse(actualCreateClasspathResult.isReference());
    assertFalse(classpath.isReference());
    assertTrue(actualCreateClasspathResult.isEmpty());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link DefBase#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor).</li>
   *   <li>Then {@link Componentdef} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenComponentdef_thenComponentdefClasspathProjectIsNull() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setClasspathRef(new Reference("42"));

    // Assert
    assertEquals("ant.loader.42", componentdef.getClasspathId());
    assertEquals("ant.loader.42", componentdef.getLoaderId());
    Path classpath = componentdef.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link DefBase#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link Componentdef} (default constructor) Classpath Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenComponentdefClasspathProjectIsProject() {
    // Arrange
    Componentdef componentdef = new Componentdef();
    Project project = new Project();
    componentdef.setProject(project);

    // Act
    componentdef.setClasspathRef(new Reference("42"));

    // Assert
    assertEquals("ant.loader.42", componentdef.getClasspathId());
    assertEquals("ant.loader.42", componentdef.getLoaderId());
    Path classpath = componentdef.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link DefBase#setLoaderRef(Reference)}.
   * <ul>
   *   <li>When {@link Reference#Reference(String)} with id is {@code 42}.</li>
   *   <li>Then {@link Componentdef} (default constructor) ClasspathId is {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#setLoaderRef(Reference)}
   */
  @Test
  public void testSetLoaderRef_whenReferenceWithIdIs42_thenComponentdefClasspathIdIs42() {
    // Arrange
    Componentdef componentdef = new Componentdef();

    // Act
    componentdef.setLoaderRef(new Reference("42"));

    // Assert
    assertEquals("42", componentdef.getClasspathId());
    assertEquals("42", componentdef.getLoaderId());
  }

  /**
   * Test {@link DefBase#createLoader()}.
   * <ul>
   *   <li>Given {@link Componentdef} (default constructor) AntlibClassLoader is {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createLoader()}
   */
  @Test
  public void testCreateLoader_givenComponentdefAntlibClassLoaderIsAntClassLoader() {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setAntlibClassLoader(new AntClassLoader());

    // Act and Assert
    assertNotNull(componentdef.createLoader());
  }

  /**
   * Test {@link DefBase#createLoader()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createLoader()}
   */
  @Test
  public void testCreateLoader_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act and Assert
    assertNotNull(componentdef.createLoader());
    assertEquals(2, componentdef.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link DefBase#createLoader()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addTarget {@code ant.PropertyHelper} and {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createLoader()}
   */
  @Test
  public void testCreateLoader_givenProjectAddTargetAntPropertyHelperAndTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act and Assert
    assertNotNull(componentdef.createLoader());
    assertEquals(2, componentdef.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link DefBase#createLoader()}.
   * <ul>
   *   <li>Then {@link Componentdef} (default constructor) Project BuildListeners size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createLoader()}
   */
  @Test
  public void testCreateLoader_thenComponentdefProjectBuildListenersSizeIsOne() {
    // Arrange
    Componentdef componentdef = new Componentdef();
    componentdef.setProject(new Project());

    // Act and Assert
    assertNotNull(componentdef.createLoader());
    assertEquals(1, componentdef.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link DefBase#createLoader()}.
   * <ul>
   *   <li>Then {@link Componentdef} (default constructor) Project BuildListeners size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefBase#createLoader()}
   */
  @Test
  public void testCreateLoader_thenComponentdefProjectBuildListenersSizeIsTwo() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Componentdef componentdef = new Componentdef();
    componentdef.setProject(project);

    // Act and Assert
    assertNotNull(componentdef.createLoader());
    assertEquals(2, componentdef.getProject().getBuildListeners().size());
  }
}
