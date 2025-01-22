package org.apache.tools.ant.listener;

import static org.junit.Assert.assertEquals;
import org.apache.tools.ant.BuildEvent;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class BigProjectLoggerDiffblueTest {
  /**
   * Test {@link BigProjectLogger#extractNameOrDefault(BuildEvent)}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>When {@link Project} (default constructor) Name is {@code Name}.</li>
   *   <li>Then return {@code "Name"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BigProjectLogger#extractNameOrDefault(BuildEvent)}
   */
  @Test
  public void testExtractNameOrDefault_givenName_whenProjectNameIsName_thenReturnName() {
    // Arrange
    BigProjectLogger bigProjectLogger = new BigProjectLogger();

    Project project = new Project();
    project.setName("Name");

    // Act and Assert
    assertEquals("\"Name\"", bigProjectLogger.extractNameOrDefault(new BuildEvent(project)));
  }

  /**
   * Test {@link BigProjectLogger#extractNameOrDefault(BuildEvent)}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link BigProjectLogger#extractNameOrDefault(BuildEvent)}
   */
  @Test
  public void testExtractNameOrDefault_thenReturnEmptyString() {
    // Arrange
    BigProjectLogger bigProjectLogger = new BigProjectLogger();

    // Act and Assert
    assertEquals("", bigProjectLogger.extractNameOrDefault(new BuildEvent(new Project())));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link BigProjectLogger#getFooter()}
   *   <li>{@link BigProjectLogger#getHeader()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    BigProjectLogger bigProjectLogger = new BigProjectLogger();

    // Act
    String actualFooter = bigProjectLogger.getFooter();

    // Assert
    assertEquals(BigProjectLogger.FOOTER, actualFooter);
    assertEquals(BigProjectLogger.FOOTER, bigProjectLogger.getHeader());
  }

  /**
   * Test new {@link BigProjectLogger} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BigProjectLogger}
   */
  @Test
  public void testNewBigProjectLogger() {
    // Arrange and Act
    BigProjectLogger actualBigProjectLogger = new BigProjectLogger();

    // Assert
    assertEquals(0, actualBigProjectLogger.getMessageOutputLevel());
    assertEquals(BigProjectLogger.FOOTER, actualBigProjectLogger.getFooter());
    assertEquals(BigProjectLogger.FOOTER, actualBigProjectLogger.getHeader());
  }
}
