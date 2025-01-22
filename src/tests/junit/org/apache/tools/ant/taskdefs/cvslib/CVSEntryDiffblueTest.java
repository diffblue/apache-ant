package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Date;
import java.util.Vector;
import org.junit.Test;

public class CVSEntryDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CVSEntry#CVSEntry(Date, String, String)}
   *   <li>{@link CVSEntry#setAuthor(String)}
   *   <li>{@link CVSEntry#toString()}
   *   <li>{@link CVSEntry#getAuthor()}
   *   <li>{@link CVSEntry#getComment()}
   *   <li>{@link CVSEntry#getDate()}
   *   <li>{@link CVSEntry#getFiles()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Date date = Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant());

    // Act
    CVSEntry actualCvsEntry = new CVSEntry(date, "JaneDoe", "Comment");
    actualCvsEntry.setAuthor("JaneDoe");
    actualCvsEntry.toString();
    String actualAuthor = actualCvsEntry.getAuthor();
    String actualComment = actualCvsEntry.getComment();
    Date actualDate = actualCvsEntry.getDate();

    // Assert
    assertEquals("Comment", actualComment);
    assertEquals("JaneDoe", actualAuthor);
    assertTrue(actualCvsEntry.getFiles().isEmpty());
    assertSame(date, actualDate);
  }

  /**
   * Test {@link CVSEntry#addFile(String, String)} with {@code file}, {@code revision}.
   * <p>
   * Method under test: {@link CVSEntry#addFile(String, String)}
   */
  @Test
  public void testAddFileWithFileRevision() {
    // Arrange
    CVSEntry cvsEntry = new CVSEntry(
        Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()), "JaneDoe", "Comment");

    // Act
    cvsEntry.addFile("File", "Revision");

    // Assert
    Vector<RCSFile> files = cvsEntry.getFiles();
    assertEquals(1, files.size());
    RCSFile getResult = files.get(0);
    assertEquals("File", getResult.getName());
    assertEquals("Revision", getResult.getRevision());
    assertNull(getResult.getPreviousRevision());
  }

  /**
   * Test {@link CVSEntry#addFile(String, String, String)} with {@code file}, {@code revision}, {@code previousRevision}.
   * <p>
   * Method under test: {@link CVSEntry#addFile(String, String, String)}
   */
  @Test
  public void testAddFileWithFileRevisionPreviousRevision() {
    // Arrange
    CVSEntry cvsEntry = new CVSEntry(
        Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()), "JaneDoe", "Comment");

    // Act
    cvsEntry.addFile("File", "Revision", "Previous Revision");

    // Assert
    Vector<RCSFile> files = cvsEntry.getFiles();
    assertEquals(1, files.size());
    RCSFile getResult = files.get(0);
    assertEquals("File", getResult.getName());
    assertEquals("Previous Revision", getResult.getPreviousRevision());
    assertEquals("Revision", getResult.getRevision());
  }

  /**
   * Test {@link CVSEntry#addFile(String, String, String)} with {@code file}, {@code revision}, {@code previousRevision}.
   * <p>
   * Method under test: {@link CVSEntry#addFile(String, String, String)}
   */
  @Test
  public void testAddFileWithFileRevisionPreviousRevision2() {
    // Arrange
    CVSEntry cvsEntry = new CVSEntry(
        Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()), "JaneDoe", "Comment");

    // Act
    cvsEntry.addFile("File", "Previous Revision", "Previous Revision");

    // Assert
    Vector<RCSFile> files = cvsEntry.getFiles();
    assertEquals(1, files.size());
    RCSFile getResult = files.get(0);
    assertEquals("File", getResult.getName());
    assertEquals("Previous Revision", getResult.getRevision());
    assertNull(getResult.getPreviousRevision());
  }
}
