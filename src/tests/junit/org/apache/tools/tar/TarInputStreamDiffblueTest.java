package org.apache.tools.tar;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import org.junit.Test;

public class TarInputStreamDiffblueTest {
  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int)}.
   * <ul>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int)}
   */
  @Test
  public void testNewTarInputStream_thenReturnReadIsMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int)}.
   * <ul>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int)}
   */
  @Test
  public void testNewTarInputStream_thenReturnReadIsMinusOne2() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, 3)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenEncoding_thenReturnReadIsMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, 3, "Encoding"))
        .read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenEncoding_thenReturnReadIsMinusOne2() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, "Encoding")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, String)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, String)}
   */
  @Test
  public void testNewTarInputStream_whenEncoding_thenReturnReadIsMinusOne3() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), "Encoding")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenNull() throws IOException {
    // Arrange, Act and Assert
    assertNull(null);
    assertEquals(-1, (new TarInputStream(null)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, String)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenNull2() throws IOException {
    // Arrange, Act and Assert
    assertNull(null);
    assertEquals(-1, (new TarInputStream(null, "UTF-8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenReturnReadIsMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TarInputStream(null, 3)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenReturnReadIsMinusOne2() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TarInputStream(null, 3, 3)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenReturnReadIsMinusOne3() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, 3, null)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenReturnReadIsMinusOne4() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TarInputStream(null, 3, 3, "UTF-8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenReturnReadIsMinusOne5() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, null)).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenNull_thenReturnReadIsMinusOne6() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TarInputStream(null, 3, "UTF-8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code UTF-8}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenUtf8_thenReturnReadIsMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, 3, "UTF-8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, int, String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenUtf8_thenReturnReadIsMinusOne2() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, 3, "UTF8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, String)}.
   * <ul>
   *   <li>When {@code UTF-8}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenUtf8_thenReturnReadIsMinusOne3() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, "UTF-8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#TarInputStream(InputStream, int, String)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then return read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#TarInputStream(InputStream, int, String)}
   */
  @Test
  public void testNewTarInputStream_whenUtf8_thenReturnReadIsMinusOne4() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3, "UTF8")).read(new byte[]{}));
  }

  /**
   * Test {@link TarInputStream#getRecordSize()}.
   * <p>
   * Method under test: {@link TarInputStream#getRecordSize()}
   */
  @Test
  public void testGetRecordSize() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(TarBuffer.DEFAULT_RCDSIZE,
        (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).getRecordSize());
  }

  /**
   * Test {@link TarInputStream#available()}.
   * <p>
   * Method under test: {@link TarInputStream#available()}
   */
  @Test
  public void testAvailable() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0, (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).available());
  }

  /**
   * Test {@link TarInputStream#skip(long)}.
   * <ul>
   *   <li>When {@code 2147483647}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#skip(long)}
   */
  @Test
  public void testSkip_when2147483647() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0L, (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).skip(2147483647L));
  }

  /**
   * Test {@link TarInputStream#skip(long)}.
   * <ul>
   *   <li>When minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#skip(long)}
   */
  @Test
  public void testSkip_whenMinusOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0L, (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).skip(-1L));
  }

  /**
   * Test {@link TarInputStream#skip(long)}.
   * <ul>
   *   <li>When one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#skip(long)}
   */
  @Test
  public void testSkip_whenOne() throws IOException {
    // Arrange, Act and Assert
    assertEquals(0L, (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).skip(1L));
  }

  /**
   * Test {@link TarInputStream#markSupported()}.
   * <p>
   * Method under test: {@link TarInputStream#markSupported()}
   */
  @Test
  public void testMarkSupported() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertFalse((new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).markSupported());
  }

  /**
   * Test {@link TarInputStream#getNextEntry()}.
   * <p>
   * Method under test: {@link TarInputStream#getNextEntry()}
   */
  @Test
  public void testGetNextEntry() throws IOException {
    // Arrange, Act and Assert
    assertNull(
        (new TarInputStream(new ByteArrayInputStream(new byte[]{TarConstants.LF_OLDNORM, 3, 'A', 3, 'A', 3, 'A', 3}), 3,
            1)).getNextEntry());
  }

  /**
   * Test {@link TarInputStream#getNextEntry()}.
   * <ul>
   *   <li>Given {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with empty array of {@code byte}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#getNextEntry()}
   */
  @Test
  public void testGetNextEntry_givenByteArrayInputStreamWithEmptyArrayOfByte_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new TarInputStream(new ByteArrayInputStream(new byte[]{}), 3)).getNextEntry());
  }

  /**
   * Test {@link TarInputStream#getLongNameData()}.
   * <p>
   * Method under test: {@link TarInputStream#getLongNameData()}
   */
  @Test
  public void testGetLongNameData() throws IOException {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{},
        (new TarInputStream(
            new ByteArrayInputStream(new byte[]{TarConstants.LF_OLDNORM, -1, 'A', -1, 'A', -1, 'A', -1}), "UTF-8"))
            .getLongNameData());
  }

  /**
   * Test {@link TarInputStream#getLongNameData()}.
   * <ul>
   *   <li>Given {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with empty array of {@code byte}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#getLongNameData()}
   */
  @Test
  public void testGetLongNameData_givenByteArrayInputStreamWithEmptyArrayOfByte_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull((new TarInputStream(new ByteArrayInputStream(new byte[]{}), 3)).getLongNameData());
  }

  /**
   * Test {@link TarInputStream#getLongNameData()}.
   * <ul>
   *   <li>Then return empty array of {@code byte}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#getLongNameData()}
   */
  @Test
  public void testGetLongNameData_thenReturnEmptyArrayOfByte() throws IOException {
    // Arrange, Act and Assert
    assertArrayEquals(new byte[]{},
        (new TarInputStream(new ByteArrayInputStream(new byte[]{'A', -1, 'A', -1, 'A', -1, 'A', -1}), "UTF-8"))
            .getLongNameData());
  }

  /**
   * Test {@link TarInputStream#parsePaxHeaders(InputStream)}.
   * <p>
   * Method under test: {@link TarInputStream#parsePaxHeaders(InputStream)}
   */
  @Test
  public void testParsePaxHeaders() throws IOException {
    // Arrange
    TarInputStream tarInputStream = new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);
    ByteArrayInputStream i = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act
    Map<String, String> actualParsePaxHeadersResult = tarInputStream.parsePaxHeaders(i);

    // Assert
    assertEquals(-1, i.read(new byte[]{}));
    assertTrue(actualParsePaxHeadersResult.isEmpty());
  }

  /**
   * Test {@link TarInputStream#parsePaxHeaders(InputStream)}.
   * <ul>
   *   <li>Then {@link ByteArrayInputStream#ByteArrayInputStream(byte[])} with {@code XAXAXAX} Bytes is {@code UTF-8} read is minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#parsePaxHeaders(InputStream)}
   */
  @Test
  public void testParsePaxHeaders_thenByteArrayInputStreamWithXaxaxaxBytesIsUtf8ReadIsMinusOne() throws IOException {
    // Arrange
    TarInputStream tarInputStream = new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);
    ByteArrayInputStream i = new ByteArrayInputStream(" XAXAXAX".getBytes("UTF-8"));

    // Act
    Map<String, String> actualParsePaxHeadersResult = tarInputStream.parsePaxHeaders(i);

    // Assert
    assertEquals(-1, i.read(new byte[]{}));
    assertTrue(actualParsePaxHeadersResult.isEmpty());
  }

  /**
   * Test {@link TarInputStream#parsePaxHeaders(InputStream)}.
   * <ul>
   *   <li>Then throw {@link IOException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#parsePaxHeaders(InputStream)}
   */
  @Test
  public void testParsePaxHeaders_thenThrowIOException() throws IOException {
    // Arrange
    TarInputStream tarInputStream = new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);

    // Act and Assert
    assertThrows(IOException.class,
        () -> tarInputStream.parsePaxHeaders(new ByteArrayInputStream(" =AXAXAX".getBytes("UTF-8"))));
  }

  /**
   * Test {@link TarInputStream#read()}.
   * <p>
   * Method under test: {@link TarInputStream#read()}
   */
  @Test
  public void testRead() throws IOException {
    // Arrange, Act and Assert
    assertEquals(-1, (new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3)).read());
  }

  /**
   * Test {@link TarInputStream#read(byte[], int, int)} with {@code byte[]}, {@code int}, {@code int}.
   * <p>
   * Method under test: {@link TarInputStream#read(byte[], int, int)}
   */
  @Test
  public void testReadWithByteIntInt() throws IOException {
    // Arrange
    TarInputStream tarInputStream = new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);

    // Act and Assert
    assertEquals(-1, tarInputStream.read("AXAXAXAX".getBytes("UTF-8"), 2, 10));
  }

  /**
   * Test {@link TarInputStream#canReadEntryData(TarEntry)}.
   * <ul>
   *   <li>Given {@code S}.</li>
   *   <li>When {@link TarEntry#TarEntry(String)} with {@code Name} LinkFlag is {@code S}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#canReadEntryData(TarEntry)}
   */
  @Test
  public void testCanReadEntryData_givenS_whenTarEntryWithNameLinkFlagIsS_thenReturnFalse()
      throws UnsupportedEncodingException {
    // Arrange
    TarInputStream tarInputStream = new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);

    TarEntry te = new TarEntry("Name");
    te.setLinkFlag((byte) 'S');

    // Act and Assert
    assertFalse(tarInputStream.canReadEntryData(te));
  }

  /**
   * Test {@link TarInputStream#canReadEntryData(TarEntry)}.
   * <ul>
   *   <li>When {@link TarEntry#TarEntry(String)} with {@code Name}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarInputStream#canReadEntryData(TarEntry)}
   */
  @Test
  public void testCanReadEntryData_whenTarEntryWithName_thenReturnTrue() throws UnsupportedEncodingException {
    // Arrange
    TarInputStream tarInputStream = new TarInputStream(new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8")), 3);

    // Act and Assert
    assertTrue(tarInputStream.canReadEntryData(new TarEntry("Name")));
  }
}
