package org.apache.tools.ant.taskdefs.optional.extension;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class LibFileSetDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LibFileSet#setIncludeImpl(boolean)}
   *   <li>{@link LibFileSet#setIncludeUrl(boolean)}
   *   <li>{@link LibFileSet#setUrlBase(String)}
   *   <li>{@link LibFileSet#getUrlBase()}
   *   <li>{@link LibFileSet#isIncludeImpl()}
   *   <li>{@link LibFileSet#isIncludeURL()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    LibFileSet libFileSet = new LibFileSet();

    // Act
    libFileSet.setIncludeImpl(true);
    libFileSet.setIncludeUrl(true);
    libFileSet.setUrlBase("https://example.org/example");
    String actualUrlBase = libFileSet.getUrlBase();
    boolean actualIsIncludeImplResult = libFileSet.isIncludeImpl();

    // Assert
    assertEquals("https://example.org/example", actualUrlBase);
    assertTrue(actualIsIncludeImplResult);
    assertTrue(libFileSet.isIncludeURL());
  }

  /**
   * Test new {@link LibFileSet} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LibFileSet}
   */
  @Test
  public void testNewLibFileSet() {
    // Arrange and Act
    LibFileSet actualLibFileSet = new LibFileSet();

    // Assert
    assertNull(actualLibFileSet.getDir());
    assertNull(actualLibFileSet.getDescription());
    assertNull(actualLibFileSet.getUrlBase());
    assertNull(actualLibFileSet.getProject());
    assertNull(actualLibFileSet.getRefid());
    assertEquals(5, actualLibFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualLibFileSet.isIncludeImpl());
    assertFalse(actualLibFileSet.isIncludeURL());
    assertFalse(actualLibFileSet.isReference());
    assertTrue(actualLibFileSet.getDefaultexcludes());
    assertTrue(actualLibFileSet.getErrorOnMissingDir());
    assertTrue(actualLibFileSet.isFilesystemOnly());
  }
}
