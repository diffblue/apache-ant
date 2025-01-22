package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import java.util.Vector;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class MailerDiffblueTest {
  /**
   * Test {@link Mailer#setHost(String)}.
   * <p>
   * Method under test: {@link Mailer#setHost(String)}
   */
  @Test
  public void testSetHost() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setHost("localhost");

    // Assert
    assertEquals("localhost", plainMailer.host);
  }

  /**
   * Test {@link Mailer#setPort(int)}.
   * <p>
   * Method under test: {@link Mailer#setPort(int)}
   */
  @Test
  public void testSetPort() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setPort(8080);

    // Assert
    assertEquals(8080, plainMailer.port);
  }

  /**
   * Test {@link Mailer#setPortExplicitlySpecified(boolean)}.
   * <p>
   * Method under test: {@link Mailer#setPortExplicitlySpecified(boolean)}
   */
  @Test
  public void testSetPortExplicitlySpecified() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setPortExplicitlySpecified(true);

    // Assert
    assertTrue(plainMailer.isPortExplicitlySpecified());
  }

  /**
   * Test {@link Mailer#isPortExplicitlySpecified()}.
   * <ul>
   *   <li>Given {@link PlainMailer} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#isPortExplicitlySpecified()}
   */
  @Test
  public void testIsPortExplicitlySpecified_givenPlainMailer_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PlainMailer()).isPortExplicitlySpecified());
  }

  /**
   * Test {@link Mailer#isPortExplicitlySpecified()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#isPortExplicitlySpecified()}
   */
  @Test
  public void testIsPortExplicitlySpecified_thenReturnTrue() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();
    plainMailer.setPortExplicitlySpecified(true);

    // Act and Assert
    assertTrue(plainMailer.isPortExplicitlySpecified());
  }

  /**
   * Test {@link Mailer#setUser(String)}.
   * <p>
   * Method under test: {@link Mailer#setUser(String)}
   */
  @Test
  public void testSetUser() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setUser("User");

    // Assert
    assertEquals("User", plainMailer.user);
  }

  /**
   * Test {@link Mailer#setPassword(String)}.
   * <p>
   * Method under test: {@link Mailer#setPassword(String)}
   */
  @Test
  public void testSetPassword() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setPassword("iloveyou");

    // Assert
    assertEquals("iloveyou", plainMailer.password);
  }

  /**
   * Test {@link Mailer#setSSL(boolean)}.
   * <p>
   * Method under test: {@link Mailer#setSSL(boolean)}
   */
  @Test
  public void testSetSSL() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setSSL(true);

    // Assert
    assertTrue(plainMailer.SSL);
  }

  /**
   * Test {@link Mailer#setEnableStartTLS(boolean)}.
   * <p>
   * Method under test: {@link Mailer#setEnableStartTLS(boolean)}
   */
  @Test
  public void testSetEnableStartTLS() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setEnableStartTLS(true);

    // Assert
    assertTrue(plainMailer.isStartTLSEnabled());
  }

  /**
   * Test {@link Mailer#isStartTLSEnabled()}.
   * <ul>
   *   <li>Given {@link PlainMailer} (default constructor) EnableStartTLS is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#isStartTLSEnabled()}
   */
  @Test
  public void testIsStartTLSEnabled_givenPlainMailerEnableStartTLSIsTrue_thenReturnTrue() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();
    plainMailer.setEnableStartTLS(true);

    // Act and Assert
    assertTrue(plainMailer.isStartTLSEnabled());
  }

  /**
   * Test {@link Mailer#isStartTLSEnabled()}.
   * <ul>
   *   <li>Given {@link PlainMailer} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#isStartTLSEnabled()}
   */
  @Test
  public void testIsStartTLSEnabled_givenPlainMailer_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PlainMailer()).isStartTLSEnabled());
  }

  /**
   * Test {@link Mailer#setMessage(Message)}.
   * <p>
   * Method under test: {@link Mailer#setMessage(Message)}
   */
  @Test
  public void testSetMessage() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setMessage(new Message());

    // Assert
    Message message = plainMailer.message;
    assertEquals("text/plain", message.getMimeType());
    assertNull(message.getDescription());
    assertNull(message.getCharset());
    assertNull(message.getProject());
    assertFalse(message.isMimeTypeSpecified());
  }

  /**
   * Test {@link Mailer#setFrom(EmailAddress)}.
   * <p>
   * Method under test: {@link Mailer#setFrom(EmailAddress)}
   */
  @Test
  public void testSetFrom() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setFrom(new EmailAddress("jane.doe@example.org"));

    // Assert
    EmailAddress emailAddress = plainMailer.from;
    assertEquals("jane.doe@example.org", emailAddress.getAddress());
    assertNull(emailAddress.getName());
  }

  /**
   * Test {@link Mailer#setReplyToList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#replyToList} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setReplyToList(Vector)}
   */
  @Test
  public void testSetReplyToList_thenPlainMailerReplyToListSizeIsOne() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);

    // Act
    plainMailer.setReplyToList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.replyToList;
    assertEquals(1, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
  }

  /**
   * Test {@link Mailer#setReplyToList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#replyToList} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setReplyToList(Vector)}
   */
  @Test
  public void testSetReplyToList_thenPlainMailerReplyToListSizeIsTwo() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);
    EmailAddress emailAddress2 = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress2);

    // Act
    plainMailer.setReplyToList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.replyToList;
    assertEquals(2, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
    assertSame(emailAddress2, emailAddressList.get(1));
  }

  /**
   * Test {@link Mailer#setReplyToList(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#replyToList} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setReplyToList(Vector)}
   */
  @Test
  public void testSetReplyToList_whenVector_thenPlainMailerReplyToListEmpty() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setReplyToList(new Vector<>());

    // Assert
    assertTrue(plainMailer.replyToList.isEmpty());
  }

  /**
   * Test {@link Mailer#setToList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#toList} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setToList(Vector)}
   */
  @Test
  public void testSetToList_thenPlainMailerToListSizeIsOne() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);

    // Act
    plainMailer.setToList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.toList;
    assertEquals(1, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
  }

  /**
   * Test {@link Mailer#setToList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#toList} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setToList(Vector)}
   */
  @Test
  public void testSetToList_thenPlainMailerToListSizeIsTwo() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);
    EmailAddress emailAddress2 = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress2);

    // Act
    plainMailer.setToList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.toList;
    assertEquals(2, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
    assertSame(emailAddress2, emailAddressList.get(1));
  }

  /**
   * Test {@link Mailer#setToList(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#toList} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setToList(Vector)}
   */
  @Test
  public void testSetToList_whenVector_thenPlainMailerToListEmpty() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setToList(new Vector<>());

    // Assert
    assertTrue(plainMailer.toList.isEmpty());
  }

  /**
   * Test {@link Mailer#setCcList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#ccList} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setCcList(Vector)}
   */
  @Test
  public void testSetCcList_thenPlainMailerCcListSizeIsOne() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);

    // Act
    plainMailer.setCcList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.ccList;
    assertEquals(1, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
  }

  /**
   * Test {@link Mailer#setCcList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#ccList} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setCcList(Vector)}
   */
  @Test
  public void testSetCcList_thenPlainMailerCcListSizeIsTwo() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);
    EmailAddress emailAddress2 = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress2);

    // Act
    plainMailer.setCcList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.ccList;
    assertEquals(2, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
    assertSame(emailAddress2, emailAddressList.get(1));
  }

  /**
   * Test {@link Mailer#setCcList(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#ccList} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setCcList(Vector)}
   */
  @Test
  public void testSetCcList_whenVector_thenPlainMailerCcListEmpty() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setCcList(new Vector<>());

    // Assert
    assertTrue(plainMailer.ccList.isEmpty());
  }

  /**
   * Test {@link Mailer#setBccList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#bccList} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setBccList(Vector)}
   */
  @Test
  public void testSetBccList_thenPlainMailerBccListSizeIsOne() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);

    // Act
    plainMailer.setBccList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.bccList;
    assertEquals(1, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
  }

  /**
   * Test {@link Mailer#setBccList(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#bccList} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setBccList(Vector)}
   */
  @Test
  public void testSetBccList_thenPlainMailerBccListSizeIsTwo() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<EmailAddress> list = new Vector<>();
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress);
    EmailAddress emailAddress2 = new EmailAddress("jane.doe@example.org");
    list.add(emailAddress2);

    // Act
    plainMailer.setBccList(list);

    // Assert
    Vector<EmailAddress> emailAddressList = plainMailer.bccList;
    assertEquals(2, emailAddressList.size());
    assertSame(emailAddress, emailAddressList.get(0));
    assertSame(emailAddress2, emailAddressList.get(1));
  }

  /**
   * Test {@link Mailer#setBccList(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#bccList} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setBccList(Vector)}
   */
  @Test
  public void testSetBccList_whenVector_thenPlainMailerBccListEmpty() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setBccList(new Vector<>());

    // Assert
    assertTrue(plainMailer.bccList.isEmpty());
  }

  /**
   * Test {@link Mailer#setFiles(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#files} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setFiles(Vector)}
   */
  @Test
  public void testSetFiles_thenPlainMailerFilesSizeIsOne() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<File> files = new Vector<>();
    files.add(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    plainMailer.setFiles(files);

    // Assert
    assertEquals(1, plainMailer.files.size());
  }

  /**
   * Test {@link Mailer#setFiles(Vector)}.
   * <ul>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#files} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setFiles(Vector)}
   */
  @Test
  public void testSetFiles_thenPlainMailerFilesSizeIsTwo() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Vector<File> files = new Vector<>();
    files.add(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    files.add(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    plainMailer.setFiles(files);

    // Assert
    assertEquals(2, plainMailer.files.size());
  }

  /**
   * Test {@link Mailer#setFiles(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#files} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setFiles(Vector)}
   */
  @Test
  public void testSetFiles_whenVector_thenPlainMailerFilesEmpty() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setFiles(new Vector<>());

    // Assert
    assertTrue(plainMailer.files.isEmpty());
  }

  /**
   * Test {@link Mailer#setSubject(String)}.
   * <p>
   * Method under test: {@link Mailer#setSubject(String)}
   */
  @Test
  public void testSetSubject() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setSubject("Hello from the Dreaming Spires");

    // Assert
    assertEquals("Hello from the Dreaming Spires", plainMailer.subject);
  }

  /**
   * Test {@link Mailer#setTask(Task)}.
   * <p>
   * Method under test: {@link Mailer#setTask(Task)}
   */
  @Test
  public void testSetTask() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();
    TaskAdapter task = new TaskAdapter();

    // Act
    plainMailer.setTask(task);

    // Assert
    Task task2 = plainMailer.task;
    assertTrue(task2 instanceof TaskAdapter);
    assertNull(((TaskAdapter) task2).getProxy());
    assertNull(task2.getDescription());
    assertNull(task2.getTaskName());
    assertNull(task2.getTaskType());
    assertNull(task2.getProject());
    assertNull(task2.getOwningTarget());
    RuntimeConfigurable runtimeConfigurableWrapper = task.getRuntimeConfigurableWrapper();
    assertSame(task, runtimeConfigurableWrapper.getProxy());
    assertSame(runtimeConfigurableWrapper, task2.getRuntimeConfigurableWrapper());
  }

  /**
   * Test {@link Mailer#setIncludeFileNames(boolean)}.
   * <p>
   * Method under test: {@link Mailer#setIncludeFileNames(boolean)}
   */
  @Test
  public void testSetIncludeFileNames() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setIncludeFileNames(true);

    // Assert
    assertTrue(plainMailer.includeFileNames);
  }

  /**
   * Test {@link Mailer#setHeaders(Vector)}.
   * <ul>
   *   <li>Given {@link Header} (default constructor) Name is {@code 42}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#headers} size is two.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setHeaders(Vector)}
   */
  @Test
  public void testSetHeaders_givenHeaderNameIs42_thenPlainMailerHeadersSizeIsTwo() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Header header = new Header();
    header.setName("Name");
    header.setValue("42");

    Header header2 = new Header();
    header2.setName("42");
    header2.setValue("Value");

    Vector<Header> v = new Vector<>();
    v.add(header2);
    v.add(header);

    // Act
    plainMailer.setHeaders(v);

    // Assert
    Vector<Header> headerList = plainMailer.headers;
    assertEquals(2, headerList.size());
    assertSame(header2, headerList.get(0));
    assertSame(header, headerList.get(1));
  }

  /**
   * Test {@link Mailer#setHeaders(Vector)}.
   * <ul>
   *   <li>Given {@link Header} (default constructor) Name is {@code Name}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#headers} size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setHeaders(Vector)}
   */
  @Test
  public void testSetHeaders_givenHeaderNameIsName_thenPlainMailerHeadersSizeIsOne() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    Header header = new Header();
    header.setName("Name");
    header.setValue("42");

    Vector<Header> v = new Vector<>();
    v.add(header);

    // Act
    plainMailer.setHeaders(v);

    // Assert
    Vector<Header> headerList = plainMailer.headers;
    assertEquals(1, headerList.size());
    assertSame(header, headerList.get(0));
  }

  /**
   * Test {@link Mailer#setHeaders(Vector)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link PlainMailer} (default constructor) {@link Mailer#headers} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#setHeaders(Vector)}
   */
  @Test
  public void testSetHeaders_whenVector_thenPlainMailerHeadersEmpty() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();

    // Act
    plainMailer.setHeaders(new Vector<>());

    // Assert
    assertTrue(plainMailer.headers.isEmpty());
  }

  /**
   * Test {@link Mailer#shouldIgnoreInvalidRecipients()}.
   * <ul>
   *   <li>Given {@link PlainMailer} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#shouldIgnoreInvalidRecipients()}
   */
  @Test
  public void testShouldIgnoreInvalidRecipients_givenPlainMailer_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new PlainMailer()).shouldIgnoreInvalidRecipients());
  }

  /**
   * Test {@link Mailer#shouldIgnoreInvalidRecipients()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Mailer#shouldIgnoreInvalidRecipients()}
   */
  @Test
  public void testShouldIgnoreInvalidRecipients_thenReturnTrue() {
    // Arrange
    PlainMailer plainMailer = new PlainMailer();
    plainMailer.setIgnoreInvalidRecipients(true);

    // Act and Assert
    assertTrue(plainMailer.shouldIgnoreInvalidRecipients());
  }
}
