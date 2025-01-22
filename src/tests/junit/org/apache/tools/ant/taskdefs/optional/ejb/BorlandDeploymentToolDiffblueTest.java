package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Hashtable;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class BorlandDeploymentToolDiffblueTest {
  /**
   * Test {@link BorlandDeploymentTool#addVendorFiles(Hashtable, String)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandDeploymentTool#addVendorFiles(Hashtable, String)}
   */
  @Test
  public void testAddVendorFiles_thenThrowBuildException() {
    // Arrange
    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    borlandDeploymentTool.setVersion(1);

    // Act and Assert
    assertThrows(BuildException.class, () -> borlandDeploymentTool.addVendorFiles(new Hashtable<>(), "Dd Prefix"));
  }

  /**
   * Test {@link BorlandDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link BorlandDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new BorlandDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name-ejb.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessOutputStream(InputStream)}.
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream() throws IOException {
    // Arrange
    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    borlandDeploymentTool.setProcessOutputStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_thenThrowBuildException() throws IOException {
    // Arrange
    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> borlandDeploymentTool.setProcessOutputStream(new FileInputStream(new FileDescriptor())));
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}.
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream() throws IOException {
    // Arrange
    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});

    // Act
    borlandDeploymentTool.setProcessErrorStream(is);

    // Assert that nothing has changed
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}.
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream2() throws IOException {
    // Arrange
    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    borlandDeploymentTool.setTask(new TaskAdapter());
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    borlandDeploymentTool.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream_givenJavaLangObject() throws IOException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    borlandDeploymentTool.setTask(task);
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    borlandDeploymentTool.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream_givenProjectAddBuildListenerAntClassLoader() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    borlandDeploymentTool.setTask(task);
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    borlandDeploymentTool.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}.
   * <ul>
   *   <li>Given {@link TaskAdapter#TaskAdapter()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link BorlandDeploymentTool#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream_givenTaskAdapterProjectIsProject() throws IOException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    BorlandDeploymentTool borlandDeploymentTool = new BorlandDeploymentTool();
    borlandDeploymentTool.setTask(task);
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    borlandDeploymentTool.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link BorlandDeploymentTool}
   *   <li>{@link BorlandDeploymentTool#setBASdtd(String)}
   *   <li>{@link BorlandDeploymentTool#setDebug(boolean)}
   *   <li>{@link BorlandDeploymentTool#setGenerateclient(boolean)}
   *   <li>{@link BorlandDeploymentTool#setJava2iiopParams(String)}
   *   <li>{@link BorlandDeploymentTool#setSuffix(String)}
   *   <li>{@link BorlandDeploymentTool#setVerify(boolean)}
   *   <li>{@link BorlandDeploymentTool#setVerifyArgs(String)}
   *   <li>{@link BorlandDeploymentTool#setVersion(int)}
   *   <li>{@link BorlandDeploymentTool#setProcessInputStream(OutputStream)}
   *   <li>{@link BorlandDeploymentTool#start()}
   *   <li>{@link BorlandDeploymentTool#stop()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws IOException {
    // Arrange and Act
    BorlandDeploymentTool actualBorlandDeploymentTool = new BorlandDeploymentTool();
    actualBorlandDeploymentTool.setBASdtd("In String");
    actualBorlandDeploymentTool.setDebug(true);
    actualBorlandDeploymentTool.setGenerateclient(true);
    actualBorlandDeploymentTool.setJava2iiopParams("Params");
    actualBorlandDeploymentTool.setSuffix("In String");
    actualBorlandDeploymentTool.setVerify(true);
    actualBorlandDeploymentTool.setVerifyArgs("Args");
    actualBorlandDeploymentTool.setVersion(1);
    actualBorlandDeploymentTool.setProcessInputStream(new ByteArrayOutputStream(1));
    actualBorlandDeploymentTool.start();
    actualBorlandDeploymentTool.stop();

    // Assert
    assertNull(actualBorlandDeploymentTool.getDestDir());
    assertNull(actualBorlandDeploymentTool.getTask());
    assertNull(actualBorlandDeploymentTool.getConfig());
  }
}
