package org.apache.tools.zip;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class Zip64RequiredExceptionDiffblueTest {
  /**
   * Test {@link Zip64RequiredException#getEntryTooBigMessage(ZipEntry)}.
   * <ul>
   *   <li>When {@link ZipEntry#ZipEntry()}.</li>
   *   <li>Then return {@code 's size exceeds the limit of 4GByte.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Zip64RequiredException#getEntryTooBigMessage(ZipEntry)}
   */
  @Test
  public void testGetEntryTooBigMessage_whenZipEntry_thenReturnSSizeExceedsTheLimitOf4GByte() {
    // Arrange, Act and Assert
    assertEquals("'s size exceeds the limit of 4GByte.", Zip64RequiredException.getEntryTooBigMessage(new ZipEntry()));
  }

  /**
   * Test {@link Zip64RequiredException#Zip64RequiredException(String)}.
   * <p>
   * Method under test: {@link Zip64RequiredException#Zip64RequiredException(String)}
   */
  @Test
  public void testNewZip64RequiredException() {
    // Arrange and Act
    Zip64RequiredException actualZip64RequiredException = new Zip64RequiredException("Just cause");

    // Assert
    assertEquals("Just cause", actualZip64RequiredException.getMessage());
    assertNull(actualZip64RequiredException.getCause());
    assertEquals(0, actualZip64RequiredException.getSuppressed().length);
  }
}
