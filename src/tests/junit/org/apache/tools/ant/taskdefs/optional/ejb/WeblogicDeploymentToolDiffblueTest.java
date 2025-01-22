package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class WeblogicDeploymentToolDiffblueTest {
  /**
   * Test {@link WeblogicDeploymentTool#createWLClasspath()}.
   * <ul>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#createWLClasspath()}
   */
  @Test
  public void testCreateWLClasspath_thenReturnLocationFileNameIsNull() {
    // Arrange
    WeblogicDeploymentTool weblogicDeploymentTool = new WeblogicDeploymentTool();
    weblogicDeploymentTool.setTask(new TaskAdapter());

    // Act
    Path actualCreateWLClasspathResult = weblogicDeploymentTool.createWLClasspath();

    // Assert
    Location location = actualCreateWLClasspathResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateWLClasspathResult.getDescription());
    assertNull(actualCreateWLClasspathResult.getProject());
    assertNull(actualCreateWLClasspathResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateWLClasspathResult.size());
    assertFalse(actualCreateWLClasspathResult.isReference());
    assertTrue(actualCreateWLClasspathResult.isEmpty());
  }

  /**
   * Test {@link WeblogicDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new WeblogicDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}.
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired() {
    // Arrange
    WeblogicDeploymentTool weblogicDeploymentTool = new WeblogicDeploymentTool();
    weblogicDeploymentTool.setTask(new TaskAdapter());
    File genericJarFile = Paths
        .get(System.getProperty("java.io.tmpdir"), "Checking if weblogic Jar needs to be rebuilt for jar ")
        .toFile();

    // Act and Assert
    assertTrue(weblogicDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_givenJavaLangObject_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WeblogicDeploymentTool weblogicDeploymentTool = new WeblogicDeploymentTool();
    weblogicDeploymentTool.setTask(task);
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(weblogicDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_givenProjectAddBuildListenerAntClassLoader_thenReturnTrue() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    WeblogicDeploymentTool weblogicDeploymentTool = new WeblogicDeploymentTool();
    weblogicDeploymentTool.setTask(task);
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(weblogicDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_givenTaskAdapterProjectIsProject_thenReturnTrue() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    WeblogicDeploymentTool weblogicDeploymentTool = new WeblogicDeploymentTool();
    weblogicDeploymentTool.setTask(task);
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(weblogicDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WeblogicDeploymentTool#isRebuildRequired(File, File)}
   */
  @Test
  public void testIsRebuildRequired_thenReturnTrue() {
    // Arrange
    WeblogicDeploymentTool weblogicDeploymentTool = new WeblogicDeploymentTool();
    weblogicDeploymentTool.setTask(new TaskAdapter());
    File genericJarFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(weblogicDeploymentTool.isRebuildRequired(genericJarFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link WeblogicDeploymentTool}
   *   <li>{@link WeblogicDeploymentTool#setArgs(String)}
   *   <li>{@link WeblogicDeploymentTool#setCompiler(String)}
   *   <li>{@link WeblogicDeploymentTool#setEJBdtd(String)}
   *   <li>{@link WeblogicDeploymentTool#setEjbcClass(String)}
   *   <li>{@link WeblogicDeploymentTool#setJvmDebugLevel(Integer)}
   *   <li>{@link WeblogicDeploymentTool#setJvmargs(String)}
   *   <li>{@link WeblogicDeploymentTool#setKeepgeneric(boolean)}
   *   <li>{@link WeblogicDeploymentTool#setNewCMP(boolean)}
   *   <li>{@link WeblogicDeploymentTool#setNoEJBC(boolean)}
   *   <li>{@link WeblogicDeploymentTool#setOutputDir(File)}
   *   <li>{@link WeblogicDeploymentTool#setRebuild(boolean)}
   *   <li>{@link WeblogicDeploymentTool#setSuffix(String)}
   *   <li>{@link WeblogicDeploymentTool#setWLClasspath(Path)}
   *   <li>{@link WeblogicDeploymentTool#setWLdtd(String)}
   *   <li>{@link WeblogicDeploymentTool#getEjbcClass()}
   *   <li>{@link WeblogicDeploymentTool#getJvmDebugLevel()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    WeblogicDeploymentTool actualWeblogicDeploymentTool = new WeblogicDeploymentTool();
    actualWeblogicDeploymentTool.setArgs("Args");
    actualWeblogicDeploymentTool.setCompiler("Compiler");
    actualWeblogicDeploymentTool.setEJBdtd("In String");
    actualWeblogicDeploymentTool.setEjbcClass("mary.somerville@example.org");
    actualWeblogicDeploymentTool.setJvmDebugLevel(1);
    actualWeblogicDeploymentTool.setJvmargs("Args");
    actualWeblogicDeploymentTool.setKeepgeneric(true);
    actualWeblogicDeploymentTool.setNewCMP(true);
    actualWeblogicDeploymentTool.setNoEJBC(true);
    actualWeblogicDeploymentTool.setOutputDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualWeblogicDeploymentTool.setRebuild(true);
    actualWeblogicDeploymentTool.setSuffix("In String");
    actualWeblogicDeploymentTool.setWLClasspath(Path.systemBootClasspath);
    actualWeblogicDeploymentTool.setWLdtd("In String");
    String actualEjbcClass = actualWeblogicDeploymentTool.getEjbcClass();
    Integer actualJvmDebugLevel = actualWeblogicDeploymentTool.getJvmDebugLevel();

    // Assert
    assertEquals("mary.somerville@example.org", actualEjbcClass);
    assertNull(actualWeblogicDeploymentTool.getDestDir());
    assertNull(actualWeblogicDeploymentTool.getTask());
    assertNull(actualWeblogicDeploymentTool.getConfig());
    assertEquals(1, actualJvmDebugLevel.intValue());
  }
}
