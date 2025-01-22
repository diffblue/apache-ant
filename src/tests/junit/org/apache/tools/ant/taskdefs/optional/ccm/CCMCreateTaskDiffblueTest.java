package org.apache.tools.ant.taskdefs.optional.ccm;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.listener.AnsiColorLogger;
import org.junit.Test;

public class CCMCreateTaskDiffblueTest {
  /**
   * Test new {@link CCMCreateTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CCMCreateTask}
   */
  @Test
  public void testNewCCMCreateTask() {
    // Arrange and Act
    CCMCreateTask actualCcmCreateTask = new CCMCreateTask();

    // Assert
    assertEquals("ccm", actualCcmCreateTask.getCcmCommand());
    assertNull(actualCcmCreateTask.getDescription());
    assertNull(actualCcmCreateTask.getTaskName());
    assertNull(actualCcmCreateTask.getTaskType());
    assertNull(actualCcmCreateTask.getComment());
    assertNull(actualCcmCreateTask.getPlatform());
    assertNull(actualCcmCreateTask.getRelease());
    assertNull(actualCcmCreateTask.getResolver());
    assertNull(actualCcmCreateTask.getSubSystem());
    assertNull(actualCcmCreateTask.getTask());
    assertNull(actualCcmCreateTask.getProject());
    assertNull(actualCcmCreateTask.getOwningTarget());
    assertEquals(Continuus.COMMAND_CREATE_TASK, actualCcmCreateTask.getCcmAction());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CCMCreateTask#setComment(String)}
   *   <li>{@link CCMCreateTask#setPlatform(String)}
   *   <li>{@link CCMCreateTask#setRelease(String)}
   *   <li>{@link CCMCreateTask#setResolver(String)}
   *   <li>{@link CCMCreateTask#setSubSystem(String)}
   *   <li>{@link CCMCreateTask#setTask(String)}
   *   <li>{@link CCMCreateTask#setProcessInputStream(OutputStream)}
   *   <li>{@link CCMCreateTask#start()}
   *   <li>{@link CCMCreateTask#stop()}
   *   <li>{@link CCMCreateTask#getComment()}
   *   <li>{@link CCMCreateTask#getPlatform()}
   *   <li>{@link CCMCreateTask#getRelease()}
   *   <li>{@link CCMCreateTask#getResolver()}
   *   <li>{@link CCMCreateTask#getSubSystem()}
   *   <li>{@link CCMCreateTask#getTask()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();

    // Act
    ccmCreateTask.setComment("foo");
    ccmCreateTask.setPlatform("foo");
    ccmCreateTask.setRelease("foo");
    ccmCreateTask.setResolver("foo");
    ccmCreateTask.setSubSystem("foo");
    ccmCreateTask.setTask("foo");
    ccmCreateTask.setProcessInputStream(new ByteArrayOutputStream(1));
    ccmCreateTask.start();
    ccmCreateTask.stop();
    String actualComment = ccmCreateTask.getComment();
    String actualPlatform = ccmCreateTask.getPlatform();
    String actualRelease = ccmCreateTask.getRelease();
    String actualResolver = ccmCreateTask.getResolver();
    String actualSubSystem = ccmCreateTask.getSubSystem();

    // Assert
    assertEquals("foo", actualComment);
    assertEquals("foo", actualPlatform);
    assertEquals("foo", actualRelease);
    assertEquals("foo", actualResolver);
    assertEquals("foo", actualSubSystem);
    assertEquals("foo", ccmCreateTask.getTask());
  }

  /**
   * Test {@link CCMCreateTask#setProcessErrorStream(InputStream)}.
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ccmCreateTask.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessErrorStream(InputStream)}.
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream2() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});

    // Act
    ccmCreateTask.setProcessErrorStream(is);

    // Assert that nothing has changed
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessErrorStream(InputStream)}.
   * <ul>
   *   <li>Given {@link CCMCreateTask} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream_givenCCMCreateTaskProjectIsProject() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ccmCreateTask.setProject(new Project());
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ccmCreateTask.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessErrorStream(InputStream)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream_givenJavaLangObject() throws IOException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ccmCreateTask.setProject(project);
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ccmCreateTask.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessErrorStream(InputStream)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessErrorStream(InputStream)}
   */
  @Test
  public void testSetProcessErrorStream_givenProjectAddBuildListenerAntClassLoader() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ccmCreateTask.setProject(project);
    ByteArrayInputStream is = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    ccmCreateTask.setProcessErrorStream(is);

    // Assert
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ByteArrayInputStream is = new ByteArrayInputStream(new byte[]{});

    // Act
    ccmCreateTask.setProcessOutputStream(is);

    // Assert that nothing has changed
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>Given {@link CCMCreateTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_givenCCMCreateTask_thenThrowBuildException() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ccmCreateTask.setProcessOutputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AnsiColorLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_givenProjectAddBuildListenerAnsiColorLogger() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AnsiColorLogger());

    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ccmCreateTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ccmCreateTask.setProcessOutputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_givenProjectAddBuildListenerAntClassLoader() throws IOException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ccmCreateTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ccmCreateTask.setProcessOutputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>Then {@link CCMCreateTask} (default constructor) Task is {@code X}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_thenCCMCreateTaskTaskIsX() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ByteArrayInputStream is = new ByteArrayInputStream(" X XAXAX".getBytes("UTF-8"));

    // Act
    ccmCreateTask.setProcessOutputStream(is);

    // Assert
    assertEquals("X", ccmCreateTask.getTask());
    assertEquals(-1, is.read(new byte[]{}));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_thenThrowBuildException() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();
    ccmCreateTask.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ccmCreateTask.setProcessOutputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>When {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code XAXAXAX} Bytes is {@code UTF-8}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_whenByteArrayInputStreamWithXaxaxaxBytesIsUtf8() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ccmCreateTask.setProcessOutputStream(new ByteArrayInputStream(" XAXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link CCMCreateTask#setProcessOutputStream(InputStream)}.
   * <ul>
   *   <li>When {@link FileInputStream#FileInputStream(FileDescriptor)} with {@link FileDescriptor#FileDescriptor()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CCMCreateTask#setProcessOutputStream(InputStream)}
   */
  @Test
  public void testSetProcessOutputStream_whenFileInputStreamWithFileDescriptor() throws IOException {
    // Arrange
    CCMCreateTask ccmCreateTask = new CCMCreateTask();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ccmCreateTask.setProcessOutputStream(new FileInputStream(new FileDescriptor())));
  }
}
