package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.email.EmailTask.Encoding;
import org.apache.tools.ant.taskdefs.optional.net.MimeMail;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class EmailTaskDiffblueTest {
  /**
   * Test Encoding {@link Encoding#getValues()}.
   * <p>
   * Method under test: {@link Encoding#getValues()}
   */
  @Test
  public void testEncodingGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{EmailTask.AUTO, EmailTask.MIME, EmailTask.UU, EmailTask.PLAIN},
        (new Encoding()).getValues());
  }

  /**
   * Test Encoding new {@link Encoding} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Encoding}
   */
  @Test
  public void testEncodingNewEncoding() {
    // Arrange and Act
    Encoding actualEncoding = new Encoding();

    // Assert
    assertNull(actualEncoding.getValue());
    assertEquals(-1, actualEncoding.getIndex());
  }

  /**
   * Test {@link EmailTask#setMessage(String)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Message is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#setMessage(String)}
   */
  @Test
  public void testSetMessage_givenEmailTaskMessageIsNull_thenThrowBuildException() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setMessage(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.setMessage("Not all who wander are lost"));
  }

  /**
   * Test {@link EmailTask#setMessageFile(File)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Message is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#setMessageFile(File)}
   */
  @Test
  public void testSetMessageFile_givenEmailTaskMessageIsNull_thenThrowBuildException() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setMessage(null);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> emailTask.setMessageFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link EmailTask#addMessage(Message)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Message is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#addMessage(Message)}
   */
  @Test
  public void testAddMessage_givenEmailTaskMessageIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setMessage(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.addMessage(new Message()));
  }

  /**
   * Test {@link EmailTask#addFrom(EmailAddress)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) From is {@code foo}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#addFrom(EmailAddress)}
   */
  @Test
  public void testAddFrom_givenEmailTaskFromIsFoo_thenThrowBuildException() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setFrom("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.addFrom(new EmailAddress("jane.doe@example.org")));
  }

  /**
   * Test {@link EmailTask#setFrom(String)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#setFrom(String)}
   */
  @Test
  public void testSetFrom_thenThrowBuildException() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.setFrom("42 Main St"));
  }

  /**
   * Test {@link EmailTask#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link FileSet#FileSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenEmailTaskAddFilesetFileSet_thenFileSetProjectIsNull() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addFileset(new FileSet());
    FileSet fs = new FileSet();

    // Act
    emailTask.addFileset(fs);

    // Assert that nothing has changed
    assertNull(emailTask.getProject());
    assertNull(fs.getProject());
  }

  /**
   * Test {@link EmailTask#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then {@link EmailTask} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenEmailTaskProjectIsProject_thenEmailTaskProjectIsProject() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    Project project = new Project();
    emailTask.setProject(project);
    FileSet fs = new FileSet();

    // Act
    emailTask.addFileset(fs);

    // Assert
    assertSame(project, emailTask.getProject());
    assertSame(project, fs.getProject());
  }

  /**
   * Test {@link EmailTask#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor).</li>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then {@link FileSet#FileSet()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenEmailTask_whenFileSet_thenFileSetProjectIsNull() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    FileSet fs = new FileSet();

    // Act
    emailTask.addFileset(fs);

    // Assert that nothing has changed
    assertNull(emailTask.getProject());
    assertNull(fs.getProject());
  }

  /**
   * Test {@link EmailTask#addFileset(FileSet)}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor).</li>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link EmailTask} (default constructor) Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#addFileset(FileSet)}
   */
  @Test
  public void testAddFileset_givenEmailTask_whenNull_thenEmailTaskProjectIsNull() {
    // Arrange
    EmailTask emailTask = new EmailTask();

    // Act
    emailTask.addFileset(null);

    // Assert that nothing has changed
    assertNull(emailTask.getProject());
  }

  /**
   * Test {@link EmailTask#createAttachments()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#createAttachments()}
   */
  @Test
  public void testCreateAttachments_givenEmailTaskAddFilesetFileSet() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addFileset(new FileSet());

    // Act
    Path actualCreateAttachmentsResult = emailTask.createAttachments();

    // Assert
    Location location = actualCreateAttachmentsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateAttachmentsResult.getDescription());
    assertNull(actualCreateAttachmentsResult.getProject());
    assertNull(actualCreateAttachmentsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateAttachmentsResult.size());
    assertFalse(actualCreateAttachmentsResult.isReference());
    assertTrue(actualCreateAttachmentsResult.isEmpty());
  }

  /**
   * Test {@link EmailTask#createAttachments()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then return Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#createAttachments()}
   */
  @Test
  public void testCreateAttachments_givenEmailTaskProjectIsProject_thenReturnProjectIsProject() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    Project project = new Project();
    emailTask.setProject(project);

    // Act and Assert
    assertSame(project, emailTask.createAttachments().getProject());
  }

  /**
   * Test {@link EmailTask#createAttachments()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor).</li>
   *   <li>Then return Location FileName is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#createAttachments()}
   */
  @Test
  public void testCreateAttachments_givenEmailTask_thenReturnLocationFileNameIsNull() {
    // Arrange and Act
    Path actualCreateAttachmentsResult = (new EmailTask()).createAttachments();

    // Assert
    Location location = actualCreateAttachmentsResult.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCreateAttachmentsResult.getDescription());
    assertNull(actualCreateAttachmentsResult.getProject());
    assertNull(actualCreateAttachmentsResult.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCreateAttachmentsResult.size());
    assertFalse(actualCreateAttachmentsResult.isReference());
    assertTrue(actualCreateAttachmentsResult.isEmpty());
  }

  /**
   * Test {@link EmailTask#createHeader()}.
   * <p>
   * Method under test: {@link EmailTask#createHeader()}
   */
  @Test
  public void testCreateHeader() {
    // Arrange and Act
    Header actualCreateHeaderResult = (new EmailTask()).createHeader();

    // Assert
    assertNull(actualCreateHeaderResult.getName());
    assertNull(actualCreateHeaderResult.getValue());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addBcc {@link EmailAddress#EmailAddress(String)} with email is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddBccEmailAddressWithEmailIsJaneDoeExampleOrg() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addBcc(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addCc {@link EmailAddress#EmailAddress(String)} with email is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddCcEmailAddressWithEmailIsJaneDoeExampleOrg() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addCc(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addFrom {@link EmailAddress#EmailAddress(String)} with email is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddFromEmailAddressWithEmailIsJaneDoeExampleOrg() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addMessage {@link Message#Message()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddMessageMessage_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addReplyTo {@link EmailAddress#EmailAddress(String)} with email is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddReplyToEmailAddressWithEmailIsJaneDoeExampleOrg() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addReplyTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addTo {@link EmailAddress#EmailAddress(String)} with email is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddToEmailAddressWithEmailIsEmptyString() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addTo(new EmailAddress(""));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addTo {@link EmailAddress#EmailAddress(String)} with email is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddToEmailAddressWithEmailIsJaneDoeExampleOrg() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) addTo {@link EmailAddress#EmailAddress(String)} with email is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskAddToEmailAddressWithEmailIsJaneDoeExampleOrg2() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.addTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Charset is {@code UTF-8}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskCharsetIsUtf8_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setCharset("UTF-8");
    emailTask.addTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) EnableStartTLS is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskEnableStartTLSIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setEnableStartTLS(true);
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Encoding is {@link Encoding} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskEncodingIsEncoding_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setEncoding(new Encoding());
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) MessageMimeType is {@link EmailTask#MIME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskMessageMimeTypeIsMime_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setMessageMimeType(EmailTask.MIME);
    emailTask.addTo(new EmailAddress("jane.doe@example.org"));
    emailTask.addFrom(new EmailAddress("jane.doe@example.org"));
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Password is {@code iloveyou}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskPasswordIsIloveyou_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setPassword("iloveyou");
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskProjectIsProject_thenThrowBuildException() {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) SSL is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskSslIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setSSL(true);
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor) User is {@link EmailTask#MIME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTaskUserIsMime_thenThrowBuildException() throws BuildException {
    // Arrange
    EmailTask emailTask = new EmailTask();
    emailTask.setUser(EmailTask.MIME);
    emailTask.addMessage(new Message());

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link EmailTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenEmailTask_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new EmailTask()).execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link MimeMail} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenMimeMail_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new MimeMail()).execute());
  }

  /**
   * Test {@link EmailTask#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailTask#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    EmailTask emailTask = new EmailTask();
    emailTask.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> emailTask.execute());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link EmailTask#setCharset(String)}
   *   <li>{@link EmailTask#setEnableStartTLS(boolean)}
   *   <li>{@link EmailTask#setFailOnError(boolean)}
   *   <li>{@link EmailTask#setIgnoreInvalidRecipients(boolean)}
   *   <li>{@link EmailTask#setIncludefilenames(boolean)}
   *   <li>{@link EmailTask#setMailhost(String)}
   *   <li>{@link EmailTask#setMessageFileInputEncoding(String)}
   *   <li>{@link EmailTask#setMessageMimeType(String)}
   *   <li>{@link EmailTask#setPassword(String)}
   *   <li>{@link EmailTask#setSSL(boolean)}
   *   <li>{@link EmailTask#setSubject(String)}
   *   <li>{@link EmailTask#setUser(String)}
   *   <li>{@link EmailTask#getCharset()}
   *   <li>{@link EmailTask#getIncludeFileNames()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    EmailTask emailTask = new EmailTask();

    // Act
    emailTask.setCharset("UTF-8");
    emailTask.setEnableStartTLS(true);
    emailTask.setFailOnError(true);
    emailTask.setIgnoreInvalidRecipients(true);
    emailTask.setIncludefilenames(true);
    emailTask.setMailhost("localhost");
    emailTask.setMessageFileInputEncoding("UTF-8");
    emailTask.setMessageMimeType("Type");
    emailTask.setPassword("iloveyou");
    emailTask.setSSL(true);
    emailTask.setSubject("Hello from the Dreaming Spires");
    emailTask.setUser("User");
    String actualCharset = emailTask.getCharset();

    // Assert
    assertEquals("UTF-8", actualCharset);
    assertTrue(emailTask.getIncludeFileNames());
  }

  /**
   * Test new {@link EmailTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link EmailTask}
   */
  @Test
  public void testNewEmailTask() {
    // Arrange and Act
    EmailTask actualEmailTask = new EmailTask();

    // Assert
    Location location = actualEmailTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualEmailTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualEmailTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualEmailTask.getTaskName());
    assertNull(actualEmailTask.getTaskType());
    assertNull(actualEmailTask.getCharset());
    assertNull(actualEmailTask.getProject());
    assertNull(actualEmailTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualEmailTask.getIncludeFileNames());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualEmailTask, runtimeConfigurableWrapper.getProxy());
  }
}
