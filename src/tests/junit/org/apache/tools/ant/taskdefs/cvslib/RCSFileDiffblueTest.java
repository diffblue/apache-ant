package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class RCSFileDiffblueTest {
  /**
   * Test {@link RCSFile#RCSFile(String, String, String)}.
   * <ul>
   *   <li>When {@code Previous Revision}.</li>
   *   <li>Then return Revision is {@code Previous Revision}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RCSFile#RCSFile(String, String, String)}
   */
  @Test
  public void testNewRCSFile_whenPreviousRevision_thenReturnRevisionIsPreviousRevision() {
    // Arrange and Act
    RCSFile actualRcsFile = new RCSFile("Name", "Previous Revision", "Previous Revision");

    // Assert
    assertEquals("Name", actualRcsFile.getName());
    assertEquals("Previous Revision", actualRcsFile.getRevision());
    assertNull(actualRcsFile.getPreviousRevision());
  }

  /**
   * Test {@link RCSFile#RCSFile(String, String)}.
   * <ul>
   *   <li>When {@code Revision}.</li>
   *   <li>Then return {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RCSFile#RCSFile(String, String)}
   */
  @Test
  public void testNewRCSFile_whenRevision_thenReturnName() {
    // Arrange and Act
    RCSFile actualRcsFile = new RCSFile("Name", "Revision");

    // Assert
    assertEquals("Name", actualRcsFile.getName());
    assertEquals("Revision", actualRcsFile.getRevision());
    assertNull(actualRcsFile.getPreviousRevision());
  }

  /**
   * Test {@link RCSFile#RCSFile(String, String, String)}.
   * <ul>
   *   <li>When {@code Revision}.</li>
   *   <li>Then return {@code Previous Revision}.</li>
   * </ul>
   * <p>
   * Method under test: {@link RCSFile#RCSFile(String, String, String)}
   */
  @Test
  public void testNewRCSFile_whenRevision_thenReturnPreviousRevision() {
    // Arrange and Act
    RCSFile actualRcsFile = new RCSFile("Name", "Revision", "Previous Revision");

    // Assert
    assertEquals("Name", actualRcsFile.getName());
    assertEquals("Previous Revision", actualRcsFile.getPreviousRevision());
    assertEquals("Revision", actualRcsFile.getRevision());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link RCSFile#getName()}
   *   <li>{@link RCSFile#getPreviousRevision()}
   *   <li>{@link RCSFile#getRevision()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    RCSFile rcsFile = new RCSFile("Name", "Revision");

    // Act
    String actualName = rcsFile.getName();
    String actualPreviousRevision = rcsFile.getPreviousRevision();

    // Assert
    assertEquals("Name", actualName);
    assertEquals("Revision", rcsFile.getRevision());
    assertNull(actualPreviousRevision);
  }
}
