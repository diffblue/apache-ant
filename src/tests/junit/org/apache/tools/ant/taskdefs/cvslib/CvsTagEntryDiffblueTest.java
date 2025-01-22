package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class CvsTagEntryDiffblueTest {
  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   *   <li>Then return Revision is {@code null}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CvsTagEntry#CvsTagEntry(String)}
   *   <li>{@link CvsTagEntry#getFile()}
   *   <li>{@link CvsTagEntry#getPreviousRevision()}
   *   <li>{@link CvsTagEntry#getRevision()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenFooTxt_thenReturnRevisionIsNull() {
    // Arrange and Act
    CvsTagEntry actualCvsTagEntry = new CvsTagEntry("foo.txt");
    String actualFile = actualCvsTagEntry.getFile();
    String actualPreviousRevision = actualCvsTagEntry.getPreviousRevision();

    // Assert
    assertEquals("foo.txt", actualFile);
    assertNull(actualPreviousRevision);
    assertNull(actualCvsTagEntry.getRevision());
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@code Prev Revision}.</li>
   *   <li>Then return PreviousRevision is {@code Prev Revision}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CvsTagEntry#CvsTagEntry(String, String, String)}
   *   <li>{@link CvsTagEntry#getFile()}
   *   <li>{@link CvsTagEntry#getPreviousRevision()}
   *   <li>{@link CvsTagEntry#getRevision()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenPrevRevision_thenReturnPreviousRevisionIsPrevRevision() {
    // Arrange and Act
    CvsTagEntry actualCvsTagEntry = new CvsTagEntry("foo.txt", "Revision", "Prev Revision");
    String actualFile = actualCvsTagEntry.getFile();
    String actualPreviousRevision = actualCvsTagEntry.getPreviousRevision();

    // Assert
    assertEquals("Prev Revision", actualPreviousRevision);
    assertEquals("Revision", actualCvsTagEntry.getRevision());
    assertEquals("foo.txt", actualFile);
  }

  /**
   * Test getters and setters.
   * <ul>
   *   <li>When {@code Revision}.</li>
   *   <li>Then return {@code Revision}.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link CvsTagEntry#CvsTagEntry(String, String)}
   *   <li>{@link CvsTagEntry#getFile()}
   *   <li>{@link CvsTagEntry#getPreviousRevision()}
   *   <li>{@link CvsTagEntry#getRevision()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters_whenRevision_thenReturnRevision() {
    // Arrange and Act
    CvsTagEntry actualCvsTagEntry = new CvsTagEntry("foo.txt", "Revision");
    String actualFile = actualCvsTagEntry.getFile();
    String actualPreviousRevision = actualCvsTagEntry.getPreviousRevision();

    // Assert
    assertEquals("Revision", actualCvsTagEntry.getRevision());
    assertEquals("foo.txt", actualFile);
    assertNull(actualPreviousRevision);
  }

  /**
   * Test {@link CvsTagEntry#toString()}.
   * <ul>
   *   <li>Given {@link CvsTagEntry#CvsTagEntry(String)} with filename is {@code foo.txt}.</li>
   *   <li>Then return {@code foo.txt was removed}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagEntry#toString()}
   */
  @Test
  public void testToString_givenCvsTagEntryWithFilenameIsFooTxt_thenReturnFooTxtWasRemoved() {
    // Arrange, Act and Assert
    assertEquals("foo.txt was removed", (new CvsTagEntry("foo.txt")).toString());
  }

  /**
   * Test {@link CvsTagEntry#toString()}.
   * <ul>
   *   <li>Then return {@code foo.txt has changed from was removed to was removed}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagEntry#toString()}
   */
  @Test
  public void testToString_thenReturnFooTxtHasChangedFromWasRemovedToWasRemoved() {
    // Arrange, Act and Assert
    assertEquals("foo.txt has changed from  was removed to  was removed",
        (new CvsTagEntry("foo.txt", " was removed", " was removed")).toString());
  }

  /**
   * Test {@link CvsTagEntry#toString()}.
   * <ul>
   *   <li>Then return {@code foo.txt is new; current revision is was removed}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagEntry#toString()}
   */
  @Test
  public void testToString_thenReturnFooTxtIsNewCurrentRevisionIsWasRemoved() {
    // Arrange, Act and Assert
    assertEquals("foo.txt is new; current revision is  was removed",
        (new CvsTagEntry("foo.txt", " was removed")).toString());
  }

  /**
   * Test {@link CvsTagEntry#toString()}.
   * <ul>
   *   <li>Then return {@code foo.txt was removed; previous revision was was removed}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsTagEntry#toString()}
   */
  @Test
  public void testToString_thenReturnFooTxtWasRemovedPreviousRevisionWasWasRemoved() {
    // Arrange, Act and Assert
    assertEquals("foo.txt was removed; previous revision was  was removed",
        (new CvsTagEntry("foo.txt", null, " was removed")).toString());
  }
}
