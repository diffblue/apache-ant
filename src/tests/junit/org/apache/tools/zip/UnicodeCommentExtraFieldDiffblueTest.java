package org.apache.tools.zip;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import java.io.UnsupportedEncodingException;
import org.junit.Test;

public class UnicodeCommentExtraFieldDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link UnicodeCommentExtraField#UnicodeCommentExtraField()}
   *   <li>{@link UnicodeCommentExtraField#getHeaderId()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    UnicodeCommentExtraField actualUnicodeCommentExtraField = new UnicodeCommentExtraField();
    ZipShort actualHeaderId = actualUnicodeCommentExtraField.getHeaderId();

    // Assert
    assertEquals(0L, actualUnicodeCommentExtraField.getNameCRC32());
    assertSame(actualUnicodeCommentExtraField.UCOM_ID, actualHeaderId);
  }

  /**
   * Test {@link UnicodeCommentExtraField#UnicodeCommentExtraField(String, byte[])}.
   * <p>
   * Method under test: {@link UnicodeCommentExtraField#UnicodeCommentExtraField(String, byte[])}
   */
  @Test
  public void testNewUnicodeCommentExtraField() throws UnsupportedEncodingException {
    // Arrange and Act
    UnicodeCommentExtraField actualUnicodeCommentExtraField = new UnicodeCommentExtraField("Comment",
        "AXAXAXAX".getBytes("UTF-8"));

    // Assert
    ZipShort centralDirectoryLength = actualUnicodeCommentExtraField.getCentralDirectoryLength();
    assertEquals(12, centralDirectoryLength.getValue());
    assertEquals(2442985691L, actualUnicodeCommentExtraField.getNameCRC32());
    assertEquals(centralDirectoryLength, actualUnicodeCommentExtraField.getLocalFileDataLength());
    ZipShort expectedHeaderId = actualUnicodeCommentExtraField.UCOM_ID;
    assertSame(expectedHeaderId, actualUnicodeCommentExtraField.getHeaderId());
    byte[] expectedUnicodeName = "Comment".getBytes("UTF-8");
    assertArrayEquals(expectedUnicodeName, actualUnicodeCommentExtraField.getUnicodeName());
    assertArrayEquals(new byte[]{'\f', 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{1, -37, 0, -99, -111, 'C', 'o', 'm', 'm', 'e', 'n', 't'},
        actualUnicodeCommentExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{1, -37, 0, -99, -111, 'C', 'o', 'm', 'm', 'e', 'n', 't'},
        actualUnicodeCommentExtraField.getLocalFileDataData());
  }

  /**
   * Test {@link UnicodeCommentExtraField#UnicodeCommentExtraField(String, byte[], int, int)}.
   * <ul>
   *   <li>Then return NameCRC32 is {@code 289145240}.</li>
   * </ul>
   * <p>
   * Method under test: {@link UnicodeCommentExtraField#UnicodeCommentExtraField(String, byte[], int, int)}
   */
  @Test
  public void testNewUnicodeCommentExtraField_thenReturnNameCRC32Is289145240() throws UnsupportedEncodingException {
    // Arrange and Act
    UnicodeCommentExtraField actualUnicodeCommentExtraField = new UnicodeCommentExtraField("Text",
        "AXAXAXAX".getBytes("UTF-8"), 1, 3);

    // Assert
    assertEquals(289145240L, actualUnicodeCommentExtraField.getNameCRC32());
    ZipShort centralDirectoryLength = actualUnicodeCommentExtraField.getCentralDirectoryLength();
    assertEquals(9, centralDirectoryLength.getValue());
    assertEquals(centralDirectoryLength, actualUnicodeCommentExtraField.getLocalFileDataLength());
    ZipShort expectedHeaderId = actualUnicodeCommentExtraField.UCOM_ID;
    assertSame(expectedHeaderId, actualUnicodeCommentExtraField.getHeaderId());
    byte[] expectedUnicodeName = "Text".getBytes("UTF-8");
    assertArrayEquals(expectedUnicodeName, actualUnicodeCommentExtraField.getUnicodeName());
    assertArrayEquals(new byte[]{'\t', 0}, centralDirectoryLength.getBytes());
    assertArrayEquals(new byte[]{1, -104, 1, '<', 17, 'T', 'e', 'x', 't'},
        actualUnicodeCommentExtraField.getCentralDirectoryData());
    assertArrayEquals(new byte[]{1, -104, 1, '<', 17, 'T', 'e', 'x', 't'},
        actualUnicodeCommentExtraField.getLocalFileDataData());
  }
}
