package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.File;
import java.nio.file.Paths;
import javax.xml.parsers.SAXParser;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.ExecutorTest;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class JonasDeploymentToolDiffblueTest {
  /**
   * Test {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}
   */
  @Test
  public void testProcessDescriptor_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("The jonasroot attribute is not set.", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.processDescriptor("A Descriptor Name", null));
  }

  /**
   * Test {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}
   */
  @Test
  public void testProcessDescriptor_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.processDescriptor("A Descriptor Name", null));
  }

  /**
   * Test {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}
   */
  @Test
  public void testProcessDescriptor_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.processDescriptor("A Descriptor Name", null));
  }

  /**
   * Test {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ExecutorTest} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}
   */
  @Test
  public void testProcessDescriptor_givenProjectAddBuildListenerExecutorTest() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ExecutorTest());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.processDescriptor("A Descriptor Name", null));
  }

  /**
   * Test {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}
   */
  @Test
  public void testProcessDescriptor_givenTaskAdapterProjectIsProject_thenThrowBuildException() {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(task);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.processDescriptor("A Descriptor Name", null));
  }

  /**
   * Test {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#processDescriptor(String, SAXParser)}
   */
  @Test
  public void testProcessDescriptor_thenThrowBuildException() {
    // Arrange
    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(new TaskAdapter());

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.processDescriptor("A Descriptor Name", null));
  }

  /**
   * Test {@link JonasDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link JonasDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new JonasDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link JonasDeploymentTool#getGenicClassName(Path)}.
   * <ul>
   *   <li>Given {@link JonasDeploymentTool} (default constructor) Task is {@link TaskAdapter#TaskAdapter()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#getGenicClassName(Path)}
   */
  @Test
  public void testGetGenicClassName_givenJonasDeploymentToolTaskIsTaskAdapter_thenReturnNull() {
    // Arrange
    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setTask(new TaskAdapter());

    // Act and Assert
    assertNull(jonasDeploymentTool.getGenicClassName(new Path(new Project())));
  }

  /**
   * Test {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}.
   * <p>
   * Method under test: {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}
   */
  @Test
  public void testCheckConfiguration() throws BuildException {
    // Arrange
    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "RMI").toFile());
    jonasDeploymentTool.setOrb(null);
    jonasDeploymentTool.setAdditionalargs(null);
    jonasDeploymentTool.setJavac(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.checkConfiguration("foo.txt", null));
  }

  /**
   * Test {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link JonasDeploymentTool} (default constructor) Additionalargs is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}
   */
  @Test
  public void testCheckConfiguration_givenJonasDeploymentToolAdditionalargsIsEmptyString() throws BuildException {
    // Arrange
    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jonasDeploymentTool.setOrb(null);
    jonasDeploymentTool.setAdditionalargs("");
    jonasDeploymentTool.setJavac(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.checkConfiguration("foo.txt", null));
  }

  /**
   * Test {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link JonasDeploymentTool} (default constructor) Javac is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}
   */
  @Test
  public void testCheckConfiguration_givenJonasDeploymentToolJavacIsEmptyString() throws BuildException {
    // Arrange
    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jonasDeploymentTool.setOrb(null);
    jonasDeploymentTool.setAdditionalargs(null);
    jonasDeploymentTool.setJavac("");

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.checkConfiguration("foo.txt", null));
  }

  /**
   * Test {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link JonasDeploymentTool} (default constructor) Orb is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}
   */
  @Test
  public void testCheckConfiguration_givenJonasDeploymentToolOrbIsFoo_thenThrowBuildException() throws BuildException {
    // Arrange
    JonasDeploymentTool jonasDeploymentTool = new JonasDeploymentTool();
    jonasDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    jonasDeploymentTool.setOrb("foo");
    jonasDeploymentTool.setAdditionalargs(null);
    jonasDeploymentTool.setJavac(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> jonasDeploymentTool.checkConfiguration("foo.txt", null));
  }

  /**
   * Test {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}.
   * <ul>
   *   <li>Given {@link JonasDeploymentTool} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JonasDeploymentTool#checkConfiguration(String, SAXParser)}
   */
  @Test
  public void testCheckConfiguration_givenJonasDeploymentTool_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new JonasDeploymentTool()).checkConfiguration("foo.txt", null));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link JonasDeploymentTool}
   *   <li>{@link JonasDeploymentTool#setAdditionalargs(String)}
   *   <li>{@link JonasDeploymentTool#setJarsuffix(String)}
   *   <li>{@link JonasDeploymentTool#setJavac(String)}
   *   <li>{@link JonasDeploymentTool#setJavacopts(String)}
   *   <li>{@link JonasDeploymentTool#setJonasroot(File)}
   *   <li>{@link JonasDeploymentTool#setKeepgenerated(boolean)}
   *   <li>{@link JonasDeploymentTool#setKeepgeneric(boolean)}
   *   <li>{@link JonasDeploymentTool#setNocompil(boolean)}
   *   <li>{@link JonasDeploymentTool#setNogenic(boolean)}
   *   <li>{@link JonasDeploymentTool#setNovalidation(boolean)}
   *   <li>{@link JonasDeploymentTool#setOrb(String)}
   *   <li>{@link JonasDeploymentTool#setRmicopts(String)}
   *   <li>{@link JonasDeploymentTool#setSecpropag(boolean)}
   *   <li>{@link JonasDeploymentTool#setVerbose(boolean)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    JonasDeploymentTool actualJonasDeploymentTool = new JonasDeploymentTool();
    actualJonasDeploymentTool.setAdditionalargs("A String");
    actualJonasDeploymentTool.setJarsuffix("A String");
    actualJonasDeploymentTool.setJavac("A String");
    actualJonasDeploymentTool.setJavacopts("A String");
    actualJonasDeploymentTool.setJonasroot(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    actualJonasDeploymentTool.setKeepgenerated(true);
    actualJonasDeploymentTool.setKeepgeneric(true);
    actualJonasDeploymentTool.setNocompil(true);
    actualJonasDeploymentTool.setNogenic(true);
    actualJonasDeploymentTool.setNovalidation(true);
    actualJonasDeploymentTool.setOrb("A String");
    actualJonasDeploymentTool.setRmicopts("A String");
    actualJonasDeploymentTool.setSecpropag(true);
    actualJonasDeploymentTool.setVerbose(true);

    // Assert
    assertNull(actualJonasDeploymentTool.getDestDir());
    assertNull(actualJonasDeploymentTool.getTask());
    assertNull(actualJonasDeploymentTool.getConfig());
  }
}
